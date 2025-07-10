-module(web_rest_callback_admin_personnel).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("web_protocol.hrl").
-include("session.hrl").
-include("limits.hrl").

%% Exported functions

-export([
    get_personnel_account/2,
    get_personnel_account_by_username/2,
    get_personnel_accounts/6
]).

%% API

-spec get_personnel_account(Id, Req) -> Response when
    Id :: integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:personnel_account(), cowboy_req:req()}.

get_personnel_account(Id, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, PersonnelAccount} = db_if_personnel:get_one(Id),
    {web_protocol:personnel_account_from_json(PersonnelAccount), Req};

get_personnel_account(_Id, _Req) ->
    % Unauthorized
    web_rest_admin_personnel:get_personnel_account_403(#forbidden_error{}).

-spec get_personnel_account_by_username(Username, Req) -> Response when
    Username :: binary(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:personnel_account(), cowboy_req:req()}.

get_personnel_account_by_username(Username, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, PersonnelAccount} = db_if_personnel:get_one_by_username(Username),
    {web_protocol:personnel_account_from_json(PersonnelAccount), Req};

get_personnel_account_by_username(_Username, _Req) ->
    % Unauthorized
    web_rest_admin_personnel_username:get_personnel_account_by_username_403(#forbidden_error{}).

-spec get_personnel_accounts(Needle, OrderBy, OrderDir, Offset, Limit, Req) -> Response when
    Needle :: binary() | 'undefined',
    OrderBy :: web_protocol:personnel_account_order_by(),
    OrderDir :: web_protocol:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:collection_slice(web_protocol:personnel_account()), cowboy_req:req()}.

get_personnel_accounts(Needle, OrderBy, OrderDir, Offset, Limit, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, PersonnelAccounts} = db_if_personnel:get(Needle, util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit)),
    {ok, Total} = db_if_personnel:get_count(Needle),
    PersonnelAccounts1 = lists:map(fun web_protocol:personnel_account_from_json/1, PersonnelAccounts),
    {#collection_slice{items = PersonnelAccounts1, total = Total}, Req};

get_personnel_accounts(_Needle, _OrderBy, _OrderDir, _Offset, _Limit, _Req) ->
    % Unauthorized
    web_rest_admin_personnels:get_personnel_accounts_403(#forbidden_error{}).

%% Local functions
