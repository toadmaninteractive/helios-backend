-module(web_rest_callback_game).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("web_protocol.hrl").
-include("session.hrl").

%% Exported functions

-export([
    get_game_item/2,
    get_game_item_list/1,
    get_game_manifest/3,
    get_game_change_logs/6,
    unlock_game_branch/3,
    get_game_build/3,
    get_game_build_manifest/3,
    get_game_builds_for_branch/5,
    get_game_build_pdb/3
]).

%% API

-spec get_game_item(Guid, Req) -> Response when
    Guid :: binary(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:game_item(), cowboy_req:req()}.

get_game_item(Guid, #{?m_session := #session{key = {?actor_client, _}, user_id = UserId}} = Req) ->
    % Client
    {ok, GameJson} = db_if_games:get_one(Guid),
    Ownership = case db_if_game_ownership:get_one(Guid, UserId) of
        {ok, #{<<"ownership">> := O}} -> binary_to_atom(O, latin1);
        {error, ?err_not_exists} -> none
    end,
    {ok, UnlockedBranchIds} = db_if_game_branch_unlocks:get_for_client(UserId),
    UnlockedBranches = gb_sets:from_list(UnlockedBranchIds),
    Game = (web_helper:to_game_item(GameJson))#game_item{jira_key = undefined, selene_key = undefined},
    {ok, Branches} = db_if_game_branches:get_all(Guid),
    AvailableBranches = lists:filter(fun
        (#{<<"build_rev">> := ?null}) -> false;
        (#{<<"platform">> := Platform}) when Platform =/= <<"windows">> -> false;
        (#{<<"is_deleted">> := true}) -> false;
        (#{<<"is_default">> := true}) -> true;
        (#{<<"is_public">> := true}) -> true;
        (#{<<"id">> := BranchId}) -> gb_sets:is_element(BranchId, UnlockedBranches)
    end, Branches),
    BranchList = [web_helper:to_game_branch_item(B) || B <- AvailableBranches],
    IsPublished = maps:get(<<"is_published">>, GameJson, false),
    IsDisabled = maps:get(<<"is_disabled">>, GameJson, false),
    IsDeleted = maps:get(<<"is_deleted">>, GameJson, false),
    CanServeGame = BranchList =/= [] andalso IsPublished andalso not IsDisabled andalso not IsDeleted,
    Game1 = ?yesno(CanServeGame, Game#game_item{branches = BranchList, ownership = Ownership}, undefined),
    {Game1, Req};

get_game_item(Guid, #{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    % Personnel
    {ok, IsGameAccessible} = db_if_games:is_accessible(Guid, UserId),
    {ok, GameJson} = db_if_games:get_one(Guid),
    Game = web_helper:to_game_item(GameJson),
    {ok, Branches} = db_if_game_branches:get_all(Guid),
    {ok, IsSuperadmin} = db_if_personnel:is_superadmin(UserId),
    {ok, AccessData} = db_if_personnel_roles:get_one(UserId, Guid),
    AvailableBranches = lists:filter(fun
        (#{<<"build_rev">> := ?null}) -> false;
        (#{<<"platform">> := Platform}) when Platform =/= <<"windows">> -> false;
        (#{<<"is_deleted">> := true}) -> false;
        (_) when IsSuperadmin -> true;
        (#{<<"id">> := BranchId}) -> web_helper:is_branch_available(BranchId, AccessData)
    end, Branches),
    BranchList = [web_helper:to_game_branch_item(B) || B <- AvailableBranches],
    IsDisabled = maps:get(<<"is_disabled">>, GameJson, false),
    IsDeleted = maps:get(<<"is_deleted">>, GameJson, false),
    CanServeGame = IsGameAccessible andalso BranchList =/= [] andalso not IsDisabled andalso not IsDeleted,
    Game1 = ?yesno(CanServeGame, Game#game_item{branches = BranchList, ownership = employee}, undefined),
    {Game1, Req};

get_game_item(_Guid, _Req) ->
    % Unauthorized
    web_rest_game:get_game_item_403(#forbidden_error{}).

-spec get_game_item_list(Req) -> Response when
    Req :: cowboy_req:req(),
    Response :: {web_protocol:game_item_list(), cowboy_req:req()}.

get_game_item_list(#{?m_session := #session{key = {?actor_client, _}, user_id = UserId}} = Req) ->
    % Client
    {ok, Games} = db_if_games:get_all(),
    {ok, OwnedGames} = db_if_game_ownership:get_for_client_all(UserId),
    {ok, UnlockedBranchIds} = db_if_game_branch_unlocks:get_for_client(UserId),
    UnlockedBranches = gb_sets:from_list(UnlockedBranchIds),
    OwnershipMap = lists:foldl(fun(#{<<"game_id">> := G, <<"ownership">> := O}, Acc) -> Acc#{G => O} end, #{}, OwnedGames),
    GameList = lists:foldr(fun(JsonItem, Acc) ->
        Game = (web_helper:to_game_item(JsonItem))#game_item{jira_key = undefined, selene_key = undefined},
        Ownership = binary_to_atom(maps:get(Game#game_item.guid, OwnershipMap, <<"none">>), latin1),
        IsPublished = maps:get(<<"is_published">>, JsonItem, false) =:= true,
        {ok, Branches} = db_if_game_branches:get_all(Game#game_item.guid),
        AvailableBranches = lists:filter(fun
            (#{<<"build_rev">> := ?null}) -> false;
            (#{<<"platform">> := Platform}) when Platform =/= <<"windows">> -> false;
            (#{<<"is_deleted">> := true}) -> false;
            (#{<<"is_default">> := true}) -> true;
            (#{<<"is_public">> := true}) -> true;
            (#{<<"id">> := BranchId}) -> gb_sets:is_element(BranchId, UnlockedBranches)
        end, Branches),
        BranchList = [web_helper:to_game_branch_item(B) || B <- AvailableBranches],
        IsDisabled = maps:get(<<"is_disabled">>, JsonItem, false),
        IsDeleted = maps:get(<<"is_deleted">>, JsonItem, false),
        CanServeGame = BranchList =/= [] andalso IsPublished andalso not IsDisabled andalso not IsDeleted,
        ?yesno(CanServeGame, [Game#game_item{branches = BranchList, ownership = Ownership}|Acc], Acc)
    end, [], Games),
    {#game_item_list{games = GameList}, Req};

get_game_item_list(#{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    % Personnel
    {ok, IsSuperadmin} = db_if_personnel:is_superadmin(UserId),
    {ok, Games} = db_if_games:get_accessible_for_platform(UserId, windows),
    GameList = lists:foldr(fun(JsonItem, Acc) ->
        Game = web_helper:to_game_item(JsonItem),
        Ownership = employee,
        {ok, Branches} = db_if_game_branches:get_all(Game#game_item.guid),
        {ok, AccessData} = db_if_personnel_roles:get_one(UserId, Game#game_item.guid),
        AvailableBranches = lists:filter(fun
            (#{<<"build_rev">> := ?null}) -> false;
            (#{<<"platform">> := Platform}) when Platform =/= <<"windows">> -> false;
            (#{<<"is_deleted">> := true}) -> false;
            (_) when IsSuperadmin -> true;
            (#{<<"id">> := BranchId}) -> web_helper:is_branch_available(BranchId, AccessData)
        end, Branches),
        BranchList = [web_helper:to_game_branch_item(B) || B <- AvailableBranches],
        IsDisabled = maps:get(<<"is_disabled">>, JsonItem, false),
        IsDeleted = maps:get(<<"is_deleted">>, JsonItem, false),
        CanServeGame = BranchList =/= [] andalso not IsDisabled andalso not IsDeleted,
        ?yesno(CanServeGame, [Game#game_item{branches = BranchList, ownership = Ownership}|Acc], Acc)
    end, [], Games),
    {#game_item_list{games = GameList}, Req};

get_game_item_list(_Req) ->
    % Unauthorized
    web_rest_game_list:get_game_item_list_403(#forbidden_error{}).

-spec get_game_manifest(Guid, Branch, Req) -> Response when
    Guid :: binary(),
    Branch :: binary(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:game_manifest(), cowboy_req:req()}.

get_game_manifest(Guid, Branch, #{?m_session := #session{key = {?actor_client, _}, user_id = UserId}} = Req) ->
    % Client
    {ok, BranchId} = db_if_game_branches:get_id_by_name(Guid, Branch),
    {ok, #{<<"is_default">> := IsDefault, <<"is_public">> := IsPublic, <<"is_deleted">> := IsDeleted}} = db_if_game_branches:get_one(BranchId),
    {ok, UnlockedBranchIds} = db_if_game_branch_unlocks:get_for_client(UserId),
    IsBranchUnlocked = lists:member(BranchId, UnlockedBranchIds),
    IsAllowed = not IsDeleted andalso (IsDefault orelse IsPublic orelse IsBranchUnlocked),
    {BuildRev, Files} = ?yesno(IsAllowed, begin
        {ok, #{<<"build_id">> := BuildId, <<"build_rev">> := BR}} = db_if_game_branches:get_one(BranchId),
        {ok, #{<<"files">> := Items}} = db_if_game_manifests:get(BuildId),
        {BR, Items}
    end, {undefined, []}),
    {#game_manifest{build_rev = BuildRev, files = [web_protocol:game_file_from_json(GF) || GF <- Files]}, Req};

get_game_manifest(Guid, Branch, #{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    % Personnel
    {ok, IsGameAccessible} = db_if_games:is_accessible(Guid, UserId),
    {ok, BranchId} = db_if_game_branches:get_id_by_name(Guid, Branch),
    {ok, IsSuperadmin} = db_if_personnel:is_superadmin(UserId),
    {ok, AccessData} = db_if_personnel_roles:get_one(UserId, Guid),
    {ok, #{<<"is_deleted">> := IsDeleted}} = db_if_game_branches:get_one(BranchId),
    IsAllowed = IsGameAccessible andalso not IsDeleted andalso (IsSuperadmin orelse web_helper:is_branch_available(BranchId, AccessData)),
    {BuildRev, Files} = ?yesno(IsAllowed, begin
        {ok, #{<<"build_id">> := BuildId, <<"build_rev">> := BR}} = db_if_game_branches:get_one(BranchId),
        {ok, #{<<"files">> := Items}} = db_if_game_manifests:get(BuildId),
        {BR, Items}
    end, []),
    {#game_manifest{build_rev = BuildRev, files = [web_protocol:game_file_from_json(GF) || GF <- Files]}, Req};

get_game_manifest(_Guid, _Branch, _Req) ->
    % Unauthorized
    web_rest_game_manifest:get_game_manifest_403(#forbidden_error{}).

-spec get_game_change_logs(Guid, Branch, Search, Offset, Limit, Req) -> Response when
    Guid :: binary(),
    Branch :: binary(),
    Search :: binary() | 'undefined',
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:collection_slice(web_protocol:change_log()), cowboy_req:req()}.

get_game_change_logs(Guid, Branch, Search, Offset, Limit, #{?m_session := #session{key = {?actor_client, _}, user_id = UserId}} = Req) ->
    % Client
    {ok, BranchId} = db_if_game_branches:get_id_by_name(Guid, Branch),
    {ok, #{<<"is_default">> := IsDefault, <<"is_public">> := IsPublic, <<"is_deleted">> := IsDeleted}} = db_if_game_branches:get_one(BranchId),
    {ok, UnlockedBranchIds} = db_if_game_branch_unlocks:get_for_client(UserId),
    IsBranchUnlocked = lists:member(BranchId, UnlockedBranchIds),
    IsAllowed = not IsDeleted andalso (IsDefault orelse IsPublic orelse IsBranchUnlocked),
    {Items, TotalCount} = ?yesno(IsAllowed, begin
        {ok, #{<<"build_id">> := BuildId}} = db_if_game_branches:get_one(BranchId),
        {ok, ChangeLogs} = db_if_game_builds:get_change_logs(Guid, BuildId, Search, Offset, Limit),
        {ok, Count} = db_if_game_builds:get_change_logs_count(Guid, BuildId, Search),
        {[web_protocol:change_log_from_json(L) || L <- ChangeLogs], Count}
    end, {[], 0}),
    {#collection_slice{items = Items, total = TotalCount}, Req};

get_game_change_logs(Guid, Branch, Search, Offset, Limit, #{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    % Personnel
    {ok, IsGameAccessible} = db_if_games:is_accessible(Guid, UserId),
    {ok, BranchId} = db_if_game_branches:get_id_by_name(Guid, Branch),
    {ok, IsSuperadmin} = db_if_personnel:is_superadmin(UserId),
    {ok, AccessData} = db_if_personnel_roles:get_one(UserId, Guid),
    {ok, #{<<"is_deleted">> := IsDeleted}} = db_if_game_branches:get_one(BranchId),
    IsAllowed = IsGameAccessible andalso not IsDeleted andalso (IsSuperadmin orelse web_helper:is_branch_available(BranchId, AccessData)),
    {Items, TotalCount} = ?yesno(IsAllowed, begin
        {ok, #{<<"build_id">> := BuildId}} = db_if_game_branches:get_one(BranchId),
        {ok, ChangeLogs} = db_if_game_builds:get_change_logs(Guid, BuildId, Search, Offset, Limit),
        {ok, Count} = db_if_game_builds:get_change_logs_count(Guid, BuildId, Search),
        {[web_protocol:change_log_from_json(L) || L <- ChangeLogs], Count}
    end, {[], 0}),
    {#collection_slice{items = Items, total = TotalCount}, Req};

get_game_change_logs(_Guid, _Branch, _Search, _Offset, _Limit, _Req) ->
    % Unauthorized
    web_rest_game_changes:get_game_change_logs_403(#forbidden_error{}).

-spec unlock_game_branch(Request, Guid, Req) -> Response when
    Request :: web_protocol:branch_unlock_request(),
    Guid :: binary(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:generic_response(), cowboy_req:req()}.

unlock_game_branch(#branch_unlock_request{password = <<>>}, _Guid, Req) ->
    {#generic_response{result = false}, Req};

unlock_game_branch(#branch_unlock_request{password = Password}, Guid, #{?m_session := #session{key = {?actor_client, _}, user_id = UserId}} = Req) ->
    Result = case db_if_game_branches:get_ids_by_password(Guid, Password) of
        {ok, BranchIds} -> [db_if_game_branch_unlocks:create(UserId, BranchId) || BranchId <- BranchIds] =/= [];
        {error, _} -> false
    end,
    {#generic_response{result = Result}, Req};

unlock_game_branch(_Request, _Guid, _Req) ->
    % Unauthorized
    web_rest_game_branch_unlock:unlock_game_branch_403(#forbidden_error{}).

-spec get_game_build(Guid, BuildRev, Req) -> Response when
    Guid :: binary(),
    BuildRev :: binary(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:build(), cowboy_req:req()}.

get_game_build(Guid, BuildRev, #{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    {ok, IsGameAccessible} = db_if_games:is_accessible(Guid, UserId),
    {ok, IsSuperadmin} = db_if_personnel:is_superadmin(UserId),
    IsAllowed = IsGameAccessible orelse IsSuperadmin,
    ?doif(not IsAllowed, web_rest_game_build:get_game_build_403(#forbidden_error{})),
    case db_if_game_builds:get_one_by_rev(Guid, BuildRev) of
        {ok, Build} -> {web_protocol:build_from_json(Build), Req};
        {error, ?err_not_exists} -> web_rest_game_build:get_game_build_404(#not_found_error{});
        {error, Reason} -> web_rest_game_build:get_game_build_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end;

get_game_build(_Guid, _BuildRev, _Req) ->
    % Unauthorized
    web_rest_game_build:get_game_build_403(#forbidden_error{}).

-spec get_game_build_manifest(Guid, BuildRev, Req) -> Response when
    Guid :: binary(),
    BuildRev :: binary(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:game_manifest(), cowboy_req:req()}.

get_game_build_manifest(Guid, BuildRev, #{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    {ok, IsGameAccessible} = db_if_games:is_accessible(Guid, UserId),
    {ok, IsSuperadmin} = db_if_personnel:is_superadmin(UserId),
    IsAllowed = IsGameAccessible orelse IsSuperadmin,
    ?doif(not IsAllowed, web_rest_game_build_manifest:get_game_build_manifest_403(#forbidden_error{})),
    BuildId = case db_if_game_builds:get_one_by_rev(Guid, BuildRev) of
        {ok, #{<<"id">> := BID}} -> BID;
        {error, ?err_not_exists} -> web_rest_game_build_manifest:get_game_build_manifest_404(#not_found_error{});
        {error, Reason} -> web_rest_game_build_manifest:get_game_build_manifest_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end,
    Files = case db_if_game_manifests:get(BuildId) of
        {ok, #{<<"files">> := Items}} -> [web_protocol:game_file_from_json(GF) || GF <- Items];
        {error, ?err_not_exists} -> web_rest_game_build_manifest:get_game_build_manifest_404(#not_found_error{});
        {error, Reason1} -> web_rest_game_build_manifest:get_game_build_manifest_500(#internal_server_error{error = util_binary:to_binary(Reason1)})
    end,
    {#game_manifest{build_rev = BuildRev, files = Files}, Req};

get_game_build_manifest(_Guid, _BuildRev, _Req) ->
    % Unauthorized
    web_rest_game_build_manifest:get_game_build_manifest_403(#forbidden_error{}).

-spec get_game_builds_for_branch(Guid, Branch, Offset, Limit, Req) -> Response when
    Guid :: binary(),
    Branch :: binary(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:collection_slice(web_protocol:build()), cowboy_req:req()}.

get_game_builds_for_branch(Guid, Branch, Offset, Limit, #{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    {ok, IsGameAccessible} = db_if_games:is_accessible(Guid, UserId),
    {ok, BranchId} = db_if_game_branches:get_id_by_name(Guid, Branch),
    {ok, IsSuperadmin} = db_if_personnel:is_superadmin(UserId),
    {ok, AccessData} = db_if_personnel_roles:get_one(UserId, Guid),
    {ok, #{<<"is_deleted">> := IsDeleted}} = db_if_game_branches:get_one(BranchId),
    IsAllowed = IsGameAccessible andalso not IsDeleted andalso (IsSuperadmin orelse web_helper:is_branch_available(BranchId, AccessData)),
    {Items, Count} = case IsAllowed of
        true ->
            {ok, Builds} = db_if_game_builds:get_for_branch(BranchId, Offset, Limit),
            {ok, Total} = db_if_game_builds:get_for_branch_count(BranchId),
            {[web_protocol:build_from_json(B) || B <- Builds], Total};
        false ->
            {[], 0}
    end,
    {#collection_slice{items = Items, total = Count}, Req};

get_game_builds_for_branch(_Guid, _Branch, _Offset, _Limit, _Req) ->
    % Unauthorized
    web_rest_game_build:get_game_build_403(#forbidden_error{}).

-spec get_game_build_pdb(Guid, BuildRev, Req) -> Response when
    Guid :: binary(),
    BuildRev :: binary(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:collection(binary()), cowboy_req:req()}.

get_game_build_pdb(Guid, BuildRev, #{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    {ok, IsGameAccessible} = db_if_games:is_accessible(Guid, UserId),
    {ok, IsSuperadmin} = db_if_personnel:is_superadmin(UserId),
    IsAllowed = IsGameAccessible orelse IsSuperadmin,
    ?doif(not IsAllowed, web_rest_game_build_pdb:get_game_build_pdb_403(#forbidden_error{})),
    PdbFileLinks = case db_if_game_builds:get_one_by_rev(Guid, BuildRev) of
        {ok, #{<<"cdn_root_url">> := CdnRootUrlBuild, <<"pdb_files">> := PdbFiles}} ->
            % Attempt to substitute unsafe HTTP with HTTPS
            {ok, CdnRootUrl} = web_config:cdn_root_url(),
            {ok, CdnRootUrlSsl} = web_config:cdn_root_url_ssl(),
            CdnRootUrlBuildFixed = binary:replace(CdnRootUrlBuild, CdnRootUrl, CdnRootUrlSsl, [global]),

            % Expand filenames
            {ok, WwwRoot} = web_config:www_root(),
            BuildRoot = filename:join([WwwRoot, Guid, BuildRev]),
            PdbFiles1 = ?yesno(is_list(PdbFiles), PdbFiles, []),
            PdbFiles2 = expand_files(PdbFiles1, BuildRoot),
            [compose_url(CdnRootUrlBuildFixed, F) || F <- lists:flatten(PdbFiles2)];
        {error, ?err_not_exists} ->
            web_rest_game_build_pdb:get_game_build_pdb_404(#not_found_error{});
        {error, Reason} ->
            web_rest_game_build_pdb:get_game_build_pdb_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end,
    {#collection{items = PdbFileLinks}, Req};

get_game_build_pdb(_Guid, _BuildRev, _Req) ->
    % Unauthorized
    web_rest_game_build_pdb:get_game_build_pdb_403(#forbidden_error{}).

%% Local functions

expand_files(PdbFiles, BuildRoot) ->
    BuildRootStr = ?yesno(is_binary(BuildRoot), binary_to_list(BuildRoot), BuildRoot),
    DeepList= lists:foldl(fun(F, Acc) ->
        F1 = ?yesno(is_binary(F), binary_to_list(F), F) ++ ".gz",
        F2 = re:replace(F1, "\\\\", "/", [{return, list}, global]),
        FL = [iolist_to_binary(FX) || FX <- filelib:wildcard(F2, BuildRootStr)],
        [FL|Acc]
    end, [], PdbFiles),
    ExistingFiles = lists:flatten(DeepList),
    lists:usort(ExistingFiles).

compose_url(BaseUrl, Path) ->
    FixedBaseUrl = ?yesno(binary:last(BaseUrl) =:= $/, BaseUrl, <<BaseUrl/binary, "/">>),
    <<FixedBaseUrl/binary, Path/binary>>.
