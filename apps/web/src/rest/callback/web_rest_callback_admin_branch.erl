-module(web_rest_callback_admin_branch).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("web_protocol.hrl").
-include("session.hrl").
-include("limits.hrl").

%% Exported functions

-export([
    get_game_branch/2,
    get_game_branches/6,
    get_all_game_branches/2,
    create_game_branch/3,
    update_game_branch/4,
    set_game_branch_build/3,
    set_game_branch_as_default/3
]).

%% API

-spec get_game_branch(Id, Req) -> Response when
    Id :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:game_branch(), cowboy_req:req()}.

get_game_branch(Id, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, GameBranch} = db_if_game_branches:get_one(Id),
    {web_protocol:game_branch_from_json(GameBranch), Req};

get_game_branch(_Id, _Req) ->
    % Unauthorized
    web_rest_admin_branch:get_game_branch_403(#forbidden_error{}).

-spec get_game_branches(Guid, OrderBy, OrderDir, Offset, Limit, Req) -> Response when
    Guid :: binary(),
    OrderBy :: web_protocol:game_branch_order_by(),
    OrderDir :: web_protocol:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:collection_slice(web_protocol:game_branch()), cowboy_req:req()}.

get_game_branches(Guid, OrderBy, OrderDir, Offset, Limit, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    OrderBy1 = web_protocol:game_branch_order_by_to_json(OrderBy),
    OrderDir1 = web_protocol:order_direction_to_json(OrderDir),
    {ok, GameBranches} = db_if_game_branches:get(Guid, OrderBy1, OrderDir1, Offset, ?slice_limit(Limit)),
    {ok, Total} = db_if_game_branches:get_count(Guid),
    GameBranches1 = lists:map(fun web_protocol:game_branch_from_json/1, GameBranches),
    {#collection_slice{items = GameBranches1, total = Total}, Req};

get_game_branches(_Guid, _OrderBy, _OrderDir, _Offset, _Limit, _Req) ->
    % Unauthorized
    web_rest_admin_branches:get_game_branches_403(#forbidden_error{}).

-spec get_all_game_branches(Guid, Req) -> Response when
    Guid :: binary(),
    Req :: cowboy_req:req(),
    Response :: {[web_protocol:game_branch()], cowboy_req:req()}.

get_all_game_branches(Guid, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, GameBranches} = db_if_game_branches:get_all(Guid),
    GameBranches1 = lists:map(fun web_protocol:game_branch_from_json/1, GameBranches),
    {GameBranches1, Req};

get_all_game_branches(_Guid, _Req) ->
    % Unauthorized
    web_rest_admin_branches_all:get_all_game_branches_403(#forbidden_error{}).

-spec create_game_branch(Request, Guid, Req) -> Response when
    Request :: web_protocol:game_branch_create_request(),
    Guid :: binary(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:game_branch_create_response(), cowboy_req:req()}.

create_game_branch(Request, Guid, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    #game_branch_create_request{
        title = Title,
        description = Description,
        password = Password,
        game_engine = GameEngine,
        platform = Platform
    } = Request,
    GameEngine1 = ?yesno(GameEngine =:= undefined, ?null, web_protocol:game_engine_to_json(GameEngine)),
    Platform1 = web_protocol:platform_to_json(Platform),
    Response = case db_if_game_branches:create(Guid, Title, Description, Password, GameEngine1, Platform1) of
        {ok, BranchId} ->
            web_notify:game_item_updated(Guid),
            {ok, GameBranch} = db_if_game_branches:get_one(BranchId),
            #game_branch_create_response{result = true, branch = web_protocol:game_branch_from_json(GameBranch)};
        {error, Reason} ->
            KnownErrors = [invalid_game_id, invalid_branch_title, branch_title_already_exists, invalid_platform],
            Error = ?yesno(lists:member(Reason, KnownErrors), Reason, failure),
            #game_branch_create_response{result = false, error = Error}
    end,
    {Response, Req};

create_game_branch(_Request, _Guid, _Req) ->
    % Unauthorized
    web_rest_admin_branches:create_game_branch_403(#forbidden_error{}).

-spec update_game_branch(Request, Id, Rev, Req) -> Response when
    Request :: web_protocol:game_branch_update_request(),
    Id :: non_neg_integer(),
    Rev :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:game_branch_update_response(), cowboy_req:req()}.

update_game_branch(Request, Id, Rev, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    Patch = web_protocol:game_branch_update_request_to_json(Request),
    Response = if
        map_size(Patch) =< 0 ->
            #game_branch_update_response{result = false, error = nothing_to_update};
        true ->
            UpdateResult = db_if_game_branches:update(Id, Rev, Patch),
            {ok, #{<<"game_id">> := Guid} = GameBranch} = db_if_game_branches:get_one(Id),
            GameBranchIgor = web_protocol:game_branch_from_json(GameBranch),
            case UpdateResult of
                ok ->
                    web_notify:game_item_updated(Guid),
                    #game_branch_update_response{result = true, branch = GameBranchIgor};
                {error, ?err_not_exists} ->
                    #game_branch_update_response{result = false, error = rev_mismatch, branch = GameBranchIgor};
                {error, Reason} ->
                    KnownErrors = [invalid_branch_title, branch_title_already_exists],
                    Error = ?yesno(lists:member(Reason, KnownErrors), Reason, failure),
                    #game_branch_update_response{result = false, error = Error}
            end
    end,
    {Response, Req};

update_game_branch(_Request, _Id, _Rev, _Req) ->
    % Unauthorized
    web_rest_admin_branch_update:update_game_branch_403(#forbidden_error{}).

-spec set_game_branch_build(Request, Id, Req) -> Response when
    Request :: web_protocol:game_branch_build_request(),
    Id :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:game_branch_build_response(), cowboy_req:req()}.

set_game_branch_build(#game_branch_build_request{build_id = BuildId}, Id, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    Response = case db_if_game_branches:assign_build_to_branch(Id, BuildId) of
        ok ->
            ok = db_if_game_branch_assignments:create(Id, BuildId),
            {ok, #{<<"game_id">> := Guid} = GameBranch} = db_if_game_branches:get_one(Id),
            web_notify:game_item_updated(Guid),
            #game_branch_build_response{result = true, branch = web_protocol:game_branch_from_json(GameBranch)};
        {error, Reason} ->
            #game_branch_build_response{result = false, error = Reason}
    end,
    {Response, Req};

set_game_branch_build(_Request, _Id, _Req) ->
    % Unauthorized
    web_rest_admin_branch_build:set_game_branch_build_403(#forbidden_error{}).

-spec set_game_branch_as_default(Request, Id, Req) -> Response when
    Request :: web_protocol:empty(),
    Id :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:generic_response(), cowboy_req:req()}.

set_game_branch_as_default(_Request, Id, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    Result = db_if_game_branches:set_default_branch(Id) =:= ok,
    ?doif(Result, begin
        {ok, Guid} = db_if_game_branches:game_id(Id),
        web_notify:game_item_updated(Guid)
    end),
    {#generic_response{result = Result}, Req};

set_game_branch_as_default(_Request, _Id, _Req) ->
    % Unauthorized
    web_rest_admin_branch_default:set_game_branch_as_default_403(#forbidden_error{}).

%% Local functions
