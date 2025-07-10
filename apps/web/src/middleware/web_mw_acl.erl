-module(web_mw_acl).

-behaviour(cowboy_middleware).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("acl.hrl").
-include("http.hrl").
-include("session.hrl").

%% Exported functions

-export([
    execute/2
]).

%% API

execute(#{?m_session := #session{key = {personnel, _}, user_id = PersonnelId}} = Req, #{handler_opts := #acl{} = ACL} = Env) ->
    Method = cowboy_req:method(Req),
    Bindings = cowboy_req:bindings(Req),
    case is_allowed(Method, Bindings, PersonnelId, ACL) of
        true -> {ok, Req, Env};
        false -> {stop, forbidden(Req)}
    end;

execute(Req, #{handler_opts := #acl{}}) ->
    {stop, forbidden(Req)};

execute(Req, Env) ->
    {ok, Req, Env}.

%% Local functions

forbidden(Req) ->
    Headers = #{<<"content-type">> => <<"application/json">>},
    Body = web_util:encode_json(#{}),
    cowboy_req:reply(?http_forbidden, Headers, Body, Req).

method_role(?get, #acl{get = Role}) -> Role;
method_role(?post, #acl{post = Role}) -> Role;
method_role(?put, #acl{put = Role}) -> Role;
method_role(?patch, #acl{patch = Role}) -> Role;
method_role(?delete, #acl{delete = Role}) -> Role;
method_role(_, _) -> undefined.

role_level(?role_consumer) -> 1;
role_level(?role_uploader) -> 2;
role_level(?role_maintainer) -> 3;
role_level(?role_admin) -> 4;
role_level(_) -> 0.

is_superadmin(PersonnelId) ->
    case db_if_personnel:is_superadmin(PersonnelId) of
        {ok, true} -> true;
        _ -> false
    end.

game_id(Bindings, #acl{game_id = GameIdBinding}) when GameIdBinding =/= undefined ->
    maps:get(GameIdBinding, Bindings, undefined);
game_id(Bindings, #acl{branch_id = BranchIdBinding}) when BranchIdBinding =/= undefined ->
    BranchId = maps:get(BranchIdBinding, Bindings, undefined),
    BranchId1 = binary_to_integer(BranchId),
    case db_if_game_branches:game_id(BranchId1) of
        {ok, GameId} -> GameId;
        _ -> undefined
    end;
game_id(Bindings, #acl{build_id = BuildIdBinding}) when BuildIdBinding =/= undefined ->
    BuildId = maps:get(BuildIdBinding, Bindings, undefined),
    BuildId1 = binary_to_integer(BuildId),
    case db_if_game_builds:game_id(BuildId1) of
        {ok, GameId} -> GameId;
        _ -> undefined
    end;
game_id(_, _) -> undefined.

is_allowed(Method, Bindings, PersonnelId, ACL) ->
    % Check if current user is superadmin
    IsSuperadmin = is_superadmin(PersonnelId),

    % Proceed with role check
    case method_role(Method, ACL) of
        % No role check required
        undefined -> true;

        % Superadmin access required
        ?role_superadmin -> IsSuperadmin;

        % Superadmin is allowed to access everyting else
        _ when IsSuperadmin -> true;

        % Check role
        MinimalAcceptableRole ->
            GameId = game_id(Bindings, ACL),
            case db_if_personnel_roles:get_one(PersonnelId, GameId) of
                {ok, #{<<"user_role">> := UserRole, <<"group_roles">> := GroupRoles}} ->
                    GroupRoles1 = [R || #{<<"role">> := R} <- maps:values(GroupRoles)],
                    Roles = [web_protocol:access_role_from_json(R) || R <- [UserRole | GroupRoles1], is_binary(R)],
                    RoleLevels = [role_level(R) || R <- lists:usort(Roles)],
                    MaxLevel = ?yesno(RoleLevels =:= [], 0, lists:max(RoleLevels)),
                    MaxLevel >= role_level(MinimalAcceptableRole);
                _ -> false
            end
    end.
