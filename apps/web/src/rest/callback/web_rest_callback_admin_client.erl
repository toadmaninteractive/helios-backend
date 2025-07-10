-module(web_rest_callback_admin_client).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("web_protocol.hrl").
-include("session.hrl").
-include("limits.hrl").

%% Exported functions

-export([
    get_client_account/2,
    get_client_accounts/6
]).

%% API

-spec get_client_account(Id, Req) -> Response when
    Id :: integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:client_account(), cowboy_req:req()}.

get_client_account(Id, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, ClientAccount} = db_if_clients:get_one(Id),
    {web_protocol:client_account_from_json(ClientAccount), Req};

get_client_account(_Id, _Req) ->
    % Unauthorized
    web_rest_admin_client:get_client_account_403(#forbidden_error{}).

-spec get_client_accounts(Needle, OrderBy, OrderDir, Offset, Limit, Req) -> Response when
    Needle :: binary() | 'undefined',
    OrderBy :: web_protocol:client_account_order_by(),
    OrderDir :: web_protocol:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:collection_slice(web_protocol:client_account()), cowboy_req:req()}.

get_client_accounts(Needle, OrderBy, OrderDir, Offset, Limit, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, ClientAccounts} = db_if_clients:get(Needle, util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit)),
    {ok, Total} = db_if_clients:get_count(Needle),
    ClientAccounts1 = lists:map(fun web_protocol:client_account_from_json/1, ClientAccounts),
    {#collection_slice{items = ClientAccounts1, total = Total}, Req};

get_client_accounts(_Needle, _OrderBy, _OrderDir, _Offset, _Limit, _Req) ->
    % Unauthorized
    web_rest_admin_clients:get_client_accounts_403(#forbidden_error{}).

%% Local functions
