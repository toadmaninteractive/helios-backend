-module(web_rest_callback_admin_personnel_roles).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("web_protocol.hrl").
-include("session.hrl").
-include("limits.hrl").

%% Exported functions

-export([
    get_personnel_account_roles/7,
    get_personnel_account_roles_for_game/7,
    set_personnel_account_role/4,
    reset_personnel_account_role/3,
    get_personnel_group_roles/7,
    get_personnel_group_roles_for_game/7,
    set_personnel_group_role/4,
    reset_personnel_group_role/3,
    get_my_roles_for_game/2,
    get_my_personnel_account_roles/6
]).

%% API

-spec get_personnel_account_roles(Id, Needle, OrderBy, OrderDir, Offset, Limit, Req) -> Result when
    Id :: non_neg_integer(),
    Needle :: binary() | 'undefined',
    OrderBy :: web_protocol:personnel_account_role_per_account_order_by(),
    OrderDir :: web_protocol:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {web_protocol:collection_slice(web_protocol:personnel_account_role_per_account()), cowboy_req:req()}.

get_personnel_account_roles(Id, Needle, OrderBy, OrderDir, Offset, Limit, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, Roles} = db_if_personnel_roles:get_for_account(Id, Needle, util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit), false),
    {ok, Total} = db_if_personnel_roles:get_for_account_count(Id, Needle, false),
    Roles1 = lists:map(fun web_protocol:personnel_account_role_from_json/1, Roles),
    {#collection_slice{items = Roles1, total = Total}, Req};

get_personnel_account_roles(_Id, _Needle, _OrderBy, _OrderDir, _Offset, _Limit, _Req) ->
    % Unauthorized
    web_rest_admin_personnel_roles:get_personnel_account_roles_403(#forbidden_error{}).

-spec get_personnel_account_roles_for_game(Guid, Needle, OrderBy, OrderDir, Offset, Limit, Req) -> Result when
    Guid :: binary(),
    Needle :: binary() | 'undefined',
    OrderBy :: web_protocol:personnel_account_role_per_game_order_by(),
    OrderDir :: web_protocol:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {web_protocol:collection_slice(web_protocol:personnel_account_role_per_game()), cowboy_req:req()}.

get_personnel_account_roles_for_game(Guid, Needle, OrderBy, OrderDir, Offset, Limit, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, Roles} = db_if_personnel_roles:get_for_game(Guid, Needle, util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit)),
    {ok, Total} = db_if_personnel_roles:get_for_game_count(Guid, Needle),
    Roles1 = lists:map(fun web_protocol:personnel_account_role_from_json/1, Roles),
    {#collection_slice{items = Roles1, total = Total}, Req};

get_personnel_account_roles_for_game(_Guid, _Needle, _OrderBy, _OrderDir, _Offset, _Limit, _Req) ->
    % Unauthorized
    web_rest_admin_game_roles_account:get_personnel_account_roles_for_game_403(#forbidden_error{}).

-spec set_personnel_account_role(Request, Id, Guid, Req) -> Result when
    Request :: web_protocol:access_role_update_request(),
    Id :: non_neg_integer(),
    Guid :: binary(),
    Req :: cowboy_req:req(),
    Result :: {web_protocol:generic_response(), cowboy_req:req()}.

set_personnel_account_role(Request, Id, Guid, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    #access_role_update_request{role = Role, is_global = IsGlobal, branch_ids = BranchIds} = Request,
    IsExtendedUpdate = is_boolean(IsGlobal) andalso is_list(BranchIds),
    Result = ?yesno(
        IsExtendedUpdate,
        db_if_personnel_roles:set(Id, Guid, Role, IsGlobal, BranchIds),
        db_if_personnel_roles:set(Id, Guid, Role)
    ) =:= ok,
    ?doif(Result, web_notify:game_item_updated(Guid, [Id])),
    {#generic_response{result = Result}, Req};

set_personnel_account_role(_Request, _Id, _Guid, _Req) ->
    % Unauthorized
    web_rest_admin_personnel_role_set:set_personnel_account_role_403(#forbidden_error{}).

-spec reset_personnel_account_role(Id, Guid, Req) -> Result when
    Id :: non_neg_integer(),
    Guid :: binary(),
    Req :: cowboy_req:req(),
    Result :: {web_protocol:generic_response(), cowboy_req:req()}.

reset_personnel_account_role(Id, Guid, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    Result = case db_if_personnel_roles:delete(Id, Guid) of
        ok ->
            web_notify:game_item_updated(Guid, [Id]),
            true;
        {error, ?err_not_exists} ->
            web_notify:game_item_updated(Guid, [Id]),
            true;
        {error, _Reason} ->
            false
    end,
    {#generic_response{result = Result}, Req};

reset_personnel_account_role(_Id, _Guid, _Req) ->
    % Unauthorized
    web_rest_admin_personnel_role_set:reset_personnel_account_role_403(#forbidden_error{}).

-spec get_personnel_group_roles(Id, Needle, OrderBy, OrderDir, Offset, Limit, Req) -> Result when
    Id :: non_neg_integer(),
    Needle :: binary() | 'undefined',
    OrderBy :: web_protocol:personnel_group_role_per_group_order_by(),
    OrderDir :: web_protocol:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {web_protocol:collection_slice(web_protocol:personnel_group_role_per_group()), cowboy_req:req()}.

get_personnel_group_roles(Id, Needle, OrderBy, OrderDir, Offset, Limit, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, Roles} = db_if_personnel_group_roles:get_for_group(Id, Needle, util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit)),
    {ok, Total} = db_if_personnel_group_roles:get_for_group_count(Id, Needle),
    Roles1 = lists:map(fun web_protocol:personnel_group_role_from_json/1, Roles),
    {#collection_slice{items = Roles1, total = Total}, Req};

get_personnel_group_roles(_Id, _Needle, _OrderBy, _OrderDir, _Offset, _Limit, _Req) ->
    % Unauthorized
    web_rest_admin_personnel_group_roles:get_personnel_group_roles_403(#forbidden_error{}).

-spec get_personnel_group_roles_for_game(Guid, Needle, OrderBy, OrderDir, Offset, Limit, Req) -> Result when
    Guid :: binary(),
    Needle :: binary() | 'undefined',
    OrderBy :: web_protocol:personnel_group_role_per_game_order_by(),
    OrderDir :: web_protocol:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {web_protocol:collection_slice(web_protocol:personnel_group_role_per_game()), cowboy_req:req()}.

get_personnel_group_roles_for_game(Guid, Needle, OrderBy, OrderDir, Offset, Limit, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, Roles} = db_if_personnel_group_roles:get_for_game(Guid, Needle, util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit)),
    {ok, Total} = db_if_personnel_group_roles:get_for_game_count(Guid, Needle),
    Roles1 = lists:map(fun web_protocol:personnel_group_role_from_json/1, Roles),
    {#collection_slice{items = Roles1, total = Total}, Req};

get_personnel_group_roles_for_game(_Guid, _Needle, _OrderBy, _OrderDir, _Offset, _Limit, _Req) ->
    % Unauthorized
    web_rest_admin_game_roles_group:get_personnel_group_roles_for_game_403(#forbidden_error{}).

-spec set_personnel_group_role(Request, Id, Guid, Req) -> Result when
    Request :: web_protocol:access_role_update_request(),
    Id :: non_neg_integer(),
    Guid :: binary(),
    Req :: cowboy_req:req(),
    Result :: {web_protocol:generic_response(), cowboy_req:req()}.

set_personnel_group_role(Request, Id, Guid, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    #access_role_update_request{role = Role, is_global = IsGlobal, branch_ids = BranchIds} = Request,
    IsExtendedUpdate = is_boolean(IsGlobal) andalso is_list(BranchIds),
    Result = ?yesno(
        IsExtendedUpdate,
        db_if_personnel_group_roles:set(Id, Guid, Role, IsGlobal, BranchIds),
        db_if_personnel_group_roles:set(Id, Guid, Role)
    ) =:= ok,
    ?doif(Result, web_notify:game_item_updated(Guid)),
    {#generic_response{result = Result}, Req};

set_personnel_group_role(_Request, _Id, _Guid, _Req) ->
    % Unauthorized
    web_rest_admin_personnel_group_role_set:set_personnel_group_role_403(#forbidden_error{}).

-spec reset_personnel_group_role(Id, Guid, Req) -> Result when
    Id :: non_neg_integer(),
    Guid :: binary(),
    Req :: cowboy_req:req(),
    Result :: {web_protocol:generic_response(), cowboy_req:req()}.

reset_personnel_group_role(Id, Guid, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    Result = case db_if_personnel_group_roles:delete(Id, Guid) of
        ok ->
            web_notify:game_item_updated(Guid),
            true;
        {error, ?err_not_exists} ->
            web_notify:game_item_updated(Guid),
            true;
        {error, _Reason} ->
            false
    end,
    {#generic_response{result = Result}, Req};

reset_personnel_group_role(_Id, _Guid, _Req) ->
    % Unauthorized
    web_rest_admin_personnel_group_role_set:reset_personnel_group_role_403(#forbidden_error{}).

-spec get_my_roles_for_game(Guid, Req) -> Result when
    Guid :: binary(),
    Req :: cowboy_req:req(),
    Result :: {web_protocol:personnel_account_role(), cowboy_req:req()}.

get_my_roles_for_game(Guid, #{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    {ok, Json} = db_if_personnel_roles:get_one(UserId, Guid),
    AccountRole = web_protocol:personnel_account_role_from_json(Json),
    {AccountRole, Req};

get_my_roles_for_game(_Guid, _Req) ->
    % Unauthorized
    web_rest_admin_game_roles_me:get_my_roles_for_game_403(#forbidden_error{}).

-spec get_my_personnel_account_roles(Needle, OrderBy, OrderDir, Offset, Limit, Req) -> Result when
    Needle :: binary() | 'undefined',
    OrderBy :: web_protocol:personnel_account_role_per_account_order_by(),
    OrderDir :: web_protocol:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {web_protocol:collection_slice(web_protocol:personnel_account_role_per_account()), cowboy_req:req()}.

get_my_personnel_account_roles(Needle, OrderBy, OrderDir, Offset, Limit, #{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    {ok, Roles} = db_if_personnel_roles:get_for_account(UserId, Needle, util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit), true),
    {ok, Total} = db_if_personnel_roles:get_for_account_count(UserId, Needle, true),
    Roles1 = lists:map(fun web_protocol:personnel_account_role_from_json/1, Roles),
    {#collection_slice{items = Roles1, total = Total}, Req};

get_my_personnel_account_roles(_Needle, _OrderBy, _OrderDir, _Offset, _Limit, _Req) ->
    % Unauthorized
    web_rest_admin_personnel_my_roles:get_my_personnel_account_roles_403(#forbidden_error{}).

%% Local functions
