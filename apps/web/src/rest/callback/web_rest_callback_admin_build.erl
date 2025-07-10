-module(web_rest_callback_admin_build).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("web_protocol.hrl").
-include("session.hrl").
-include("limits.hrl").

%% Exported functions

-export([
    get_build/2,
    get_build_by_rev/3,
    get_builds/6,
    get_builds_for_game/6,
    update_build/4,
    delete_build/2,
    create_draft_build/3,
    update_draft_build/4,
    publish_draft_build/3,
    get_build_manifest/2
]).

%% API

-spec get_build(Id, Req) -> Response when
    Id :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:build(), cowboy_req:req()}.

get_build(Id, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, Build} = db_if_game_builds:get_one(Id),
    {web_protocol:build_from_json(Build), Req};

get_build(_Id, _Req) ->
    % Unauthorized
    web_rest_admin_build:get_build_403(#forbidden_error{}).

-spec get_build_by_rev(Guid, BuildRev, Req) -> Response when
    Guid :: binary(),
    BuildRev :: binary(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:build(), cowboy_req:req()}.

get_build_by_rev(Guid, BuildRev, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, Build} = db_if_game_builds:get_one_by_rev(Guid, BuildRev),
    {web_protocol:build_from_json(Build), Req};

get_build_by_rev(_Guid, _BuildRev, _Req) ->
    % Unauthorized
    web_rest_admin_build_rev:get_build_by_rev_403(#forbidden_error{}).

-spec get_builds(OrderBy, OrderDir, Offset, Limit, ActiveOnly, Req) -> Response when
    OrderBy :: web_protocol:build_order_by(),
    OrderDir :: web_protocol:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    ActiveOnly :: boolean() | 'undefined',
    Req :: cowboy_req:req(),
    Response :: {web_protocol:collection_slice(web_protocol:build()), cowboy_req:req()}.

get_builds(OrderBy, OrderDir, Offset, Limit, _ActiveOnly, #{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    OrderBy1 = util_binary:to_binary(OrderBy),
    OrderDir1 = util_binary:to_binary(OrderDir),
    {ok, Builds} = db_if_game_builds:get_for_employee(UserId, OrderBy1, OrderDir1, Offset, ?slice_limit(Limit)),
    {ok, Total} = db_if_game_builds:get_for_employee_count(UserId),
    Builds1 = lists:map(fun web_protocol:build_from_json/1, Builds),
    {#collection_slice{items = Builds1, total = Total}, Req};

get_builds(_OrderBy, _OrderDir, _Offset, _Limit, _ActiveOnly, _Req) ->
    % Unauthorized
    web_rest_admin_builds:get_builds_403(#forbidden_error{}).

-spec get_builds_for_game(Guid, OrderBy, OrderDir, Offset, Limit, Req) -> Response when
    Guid :: binary(),
    OrderBy :: web_protocol:build_order_by(),
    OrderDir :: web_protocol:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:collection_slice(web_protocol:build()), cowboy_req:req()}.

get_builds_for_game(Guid, OrderBy, OrderDir, Offset, Limit, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, Builds} = db_if_game_builds:get_for_game(Guid, undefined, util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit)),
    {ok, Total} = db_if_game_builds:get_for_game_count(Guid, undefined),
    Builds1 = lists:map(fun web_protocol:build_from_json/1, Builds),
    {#collection_slice{items = Builds1, total = Total}, Req};

get_builds_for_game(_Guid, _OrderBy, _OrderDir, _Offset, _Limit, _Req) ->
    % Unauthorized
    web_rest_admin_builds_game:get_builds_for_game_403(#forbidden_error{}).

-spec update_build(Request, Id, Rev, Req) -> Response when
    Request :: web_protocol:build_update_request(),
    Id :: non_neg_integer(),
    Rev :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:build_update_response(), cowboy_req:req()}.

update_build(Request, Id, Rev, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    Patch = web_protocol:build_update_request_to_json(Request),
    FilterAbsentFun = fun(_K, V) -> V =/= ?null end,
    Patch1 = maps:filter(FilterAbsentFun, Patch),
    UpdateResult = db_if_game_builds:update(Id, Rev, Patch1),
    {ok, #{<<"game_id">> := Guid} = Build} = db_if_game_builds:get_one(Id),
    Build1 = web_protocol:build_from_json(Build),
    Response = if
        map_size(Patch1) =< 0 ->
            #build_update_response{result = false, error = nothing_to_update};
        true ->
            case UpdateResult of
                ok ->
                    web_notify:game_item_updated(Guid),
                    #build_update_response{result = true, build = Build1};
                {error, ?err_not_exists} ->
                    #build_update_response{result = false, error = rev_mismatch, build = Build1};
                {error, _} ->
                    #build_update_response{result = false, error = failure}
            end
    end,
    {Response, Req};

update_build(_Request, _Id, _Rev, _Req) ->
    % Unauthorized
    web_rest_admin_build_update:update_build_403(#forbidden_error{}).

-spec delete_build(Id, Req) -> Response when
    Id :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:build_delete_response(), cowboy_req:req()}.

delete_build(Id, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    Response = case db_if_game_builds:get_one(Id) of
        {ok, #{<<"is_deleted">> := true}} ->
            #build_delete_response{result = false, error = already_deleted};
        {ok, #{<<"game_id">> := GameId, <<"build_rev">> := BuildRev}} ->
            case db_if_game_builds:has_assigned_branches(Id) of
                {ok, true} ->
                    #build_delete_response{result = false, error = has_assigned_branches};
                {ok, false} ->
                    {ok, WwwRoot} = web_config:www_root(),
                    BuildDir = filename:join([WwwRoot, GameId, BuildRev]),
                    ?doif(filelib:is_dir(BuildDir), util_file:rm_rf(BuildDir)),
                    db_if_game_manifests:delete(Id),
                    db_if_game_build_files:delete_all(Id),
                    case db_if_game_builds:wipe(Id) of
                        ok ->
                            #build_delete_response{result = true};
                        {error, _Reason} ->
                            db_if_game_builds:delete(Id),
                            {ok, Build} = db_if_game_builds:get_one(Id),
                            #build_delete_response{result = false, error = failure, build = web_protocol:build_from_json(Build)}
                    end;
                {error, _Reason} ->
                    #build_delete_response{result = false, error = failure}
            end;
        {error, _Reason} ->
            #build_delete_response{result = false, error = failure}
    end,
    {Response, Req};

delete_build(_Id, _Req) ->
    % Unauthorized
    web_rest_admin_build:delete_build_403(#forbidden_error{}).

-spec create_draft_build(Request, Guid, Req) -> Response when
    Request :: web_protocol:draft_build_create_request(),
    Guid :: binary(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:draft_build_create_response(), cowboy_req:req()}.

create_draft_build(#draft_build_create_request{build_rev = BuildRev, platform = Platform}, Guid, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, CdnRootUrl} = web_config:cdn_root_url(),
    CdnRootUrl1 = util_binary:join([CdnRootUrl, Guid, BuildRev, <<>>], <<"/">>),
    Response = case db_if_game_builds:create_draft(Guid, BuildRev, CdnRootUrl1, Platform) of
        {ok, BuildId} ->
            {ok, Build} = db_if_game_builds:get_one(BuildId),
            #draft_build_create_response{result = true, build = web_protocol:build_from_json(Build)};
        {error, Reason} ->
            KnownErrors = [invalid_game_id, invalid_build_rev, build_rev_already_exists, invalid_platform],
            Error = ?yesno(lists:member(Reason, KnownErrors), Reason, failure),
            #draft_build_create_response{result = false, error = Error}
    end,
    {Response, Req};

create_draft_build(_Request, _Guid, _Req) ->
    % Unauthorized
    web_rest_admin_build_create_draft:create_draft_build_403(#forbidden_error{}).

-spec update_draft_build(Request, Id, Rev, Req) -> Response when
    Request :: web_protocol:draft_build_update_request(),
    Id :: non_neg_integer(),
    Rev :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:draft_build_update_response(), cowboy_req:req()}.

update_draft_build(Request, Id, Rev, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    Patch = web_protocol:draft_build_update_request_to_json(Request),
    FilterAbsentFun = fun(_K, V) -> V =/= ?null end,
    Patch1 = maps:filter(FilterAbsentFun, Patch),
    UpdateResult = db_if_game_builds:update(Id, Rev, Patch1),
    {ok, Build} = db_if_game_builds:get_one(Id),
    Build1 = web_protocol:build_from_json(Build),
    Response = if
        map_size(Patch1) =< 0 ->
            #draft_build_update_response{result = false, error = nothing_to_update};
        true ->
            case UpdateResult of
                ok -> #draft_build_update_response{result = true, build = Build1};
                {error, ?err_not_exists} -> #draft_build_update_response{result = false, error = rev_mismatch, build = Build1};
                {error, _} -> #draft_build_update_response{result = false, error = failure}
            end
    end,
    {Response, Req};

update_draft_build(_Request, _Id, _Rev, _Req) ->
    % Unauthorized
    web_rest_admin_build_update_draft:update_draft_build_403(#forbidden_error{}).

-spec publish_draft_build(Request, Id, Req) -> Response when
    Request :: web_protocol:empty(),
    Id :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:build_publish_response(), cowboy_req:req()}.

publish_draft_build(#empty{}, Id, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    Response = case db_if_game_builds:publish_draft(Id) of
        ok ->
            {ok, Build} = db_if_game_builds:get_one(Id),
            #build_publish_response{result = true, build = web_protocol:build_from_json(Build)};
        {error, Reason} ->
            #build_publish_response{result = false, error = Reason}
    end,
    {Response, Req};

publish_draft_build(_Request, _Id, _Req) ->
    % Unauthorized
    web_rest_admin_build_publish_draft:publish_draft_build_403(#forbidden_error{}).

-spec get_build_manifest(Id, Req) -> Response when
    Id :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:game_manifest(), cowboy_req:req()}.

get_build_manifest(Id, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, #{<<"build_rev">> := BuildRev}} = db_if_game_builds:get_one(Id),
    {ok, #{<<"files">> := Files}} = db_if_game_manifests:get(Id),
    GameFiles = [web_protocol:game_file_from_json(BF) || BF <- Files],
    GameManifest = #game_manifest{build_rev = BuildRev, files = GameFiles},
    {GameManifest, Req};

get_build_manifest(_Id, _Req) ->
    % Unauthorized
    web_rest_admin_build_publish_draft:publish_draft_build_403(#forbidden_error{}).

%% Local functions
