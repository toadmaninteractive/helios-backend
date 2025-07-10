-module(db_if_client_requests).

%% Include files

-include("protocol.hrl").

%% Exported functions

-export([
    % Generic
    get/2,
    set/5,
    delete/2,
    cleanup_expired/0,

    % Shortcuts
    register_confirm/1,
    set_register_confirm/2,
    delete_register_confirm/1,
    phone_confirm/1,
    set_phone_confirm/3,
    delete_phone_confirm/1,
    password_reset_confirm/1,
    set_password_reset_confirm/4,
    delete_password_reset_confirm/1
]).

%% API

-spec get(ClientId :: non_neg_integer(), Request :: binary() | atom()) ->
    {'ok', Data :: jsx:json_term()} | {'error', Reason :: atom()}.

get(ClientId, Request) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM client_requests AS clir ",
        (common_joins())/binary,
        "WHERE clir.client_id = $1 AND clir.request = $2 "
    >>,
    Params = [ClientId, util_binary:to_binary(Request)],
    case db_query:select_one(Query, Params) of
        {ok, Data} -> {ok, Data};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec set(ClientId, Request, SecurityCode, Properties, Lifetime) -> Result when
    ClientId :: non_neg_integer(),
    Request :: binary() | atom(),
    SecurityCode :: binary(),
    Properties :: jsx:json_term(),
    Lifetime :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

set(ClientId, Request, SecurityCode, Properties, Lifetime) ->
    Query = <<
        "INSERT INTO client_requests AS clir ",
            "(client_id, request, security_code, properties, valid_thru) ",
        "VALUES ",
            "($1, $2, $3, $4, NOW() + make_interval(secs => $5::integer)) ",
        "ON CONFLICT (\"client_id\", \"request\") DO UPDATE ",
        "SET ",
            "security_code = EXCLUDED.security_code, ",
            "properties = EXCLUDED.properties, ",
            "created_at = current_timestamp, ",
            "valid_thru = EXCLUDED.valid_thru"
    >>,
    Params = [ClientId, util_binary:to_binary(Request), SecurityCode, jsx:encode(Properties), Lifetime],
    case db_query:update(Query, Params) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec delete(ClientId :: non_neg_integer(), Request :: binary() | atom()) ->
    'ok' | {'error', Reason :: atom()}.

delete(ClientId, Request) ->
    Query = <<"DELETE FROM client_requests WHERE client_id = $1 AND request = $2">>,
    Params = [ClientId, util_binary:to_binary(Request)],
    case db_query:delete(Query, Params) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec cleanup_expired() ->
    {'ok', NumDeleted :: non_neg_integer()} | {'error', Reason :: atom()}.

cleanup_expired() ->
    Query = <<"DELETE FROM client_requests WHERE valid_thru < NOW()">>,
    case db_query:delete(Query, []) of
        {ok, NumDeleted} -> {ok, NumDeleted};
        {error, Reason} -> {error, Reason}
    end.

-spec register_confirm(ClientId :: non_neg_integer()) ->
    {'ok', SecurityCode :: binary()} | {'error', Reason :: atom()}.

register_confirm(ClientId) ->
    case get(ClientId, ?cr_register_confirm) of
        {ok, #{<<"security_code">> := SecurityCode}} -> {ok, SecurityCode};
        {error, _Reason} = Error -> Error
    end.

-spec set_register_confirm(ClientId :: non_neg_integer(), SecurityCode :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

set_register_confirm(ClientId, SecurityCode) ->
    {ok, Lifetime} = db_if_settings:register_confirm_code_lifetime(),
    set(ClientId, ?cr_register_confirm, SecurityCode, #{}, Lifetime).

-spec delete_register_confirm(ClientId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

delete_register_confirm(ClientId) ->
    delete(ClientId, ?cr_register_confirm).

-spec phone_confirm(ClientId :: non_neg_integer()) ->
    {'ok', SecurityCode :: binary(), NewPhone :: binary() | 'null'} | {'error', Reason :: atom()}.

phone_confirm(ClientId) ->
    case get(ClientId, ?cr_phone_confirm) of
        {ok, #{<<"security_code">> := SecurityCode, <<"properties">> := #{<<"new_phone">> := NewPhone}}} ->
            {ok, SecurityCode, NewPhone};
        {error, _Reason} = Error ->
            Error
    end.

-spec set_phone_confirm(ClientId :: non_neg_integer(), SecurityCode :: binary(), NewPhone :: binary() | 'null') ->
    'ok' | {'error', Reason :: atom()}.

set_phone_confirm(ClientId, SecurityCode, NewPhone) ->
    {ok, Lifetime} = db_if_settings:phone_confirm_code_lifetime(),
    set(ClientId, ?cr_phone_confirm, SecurityCode, #{new_phone => NewPhone}, Lifetime).

-spec delete_phone_confirm(ClientId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

delete_phone_confirm(ClientId) ->
    delete(ClientId, ?cr_phone_confirm).

-spec password_reset_confirm(ClientId :: non_neg_integer()) ->
    {'ok', SecurityCode :: binary(), NewPassword :: binary(), NewSalt :: binary()} | {'error', Reason :: atom()}.

password_reset_confirm(ClientId) ->
    case get(ClientId, ?cr_password_reset_confirm) of
        {ok, #{<<"security_code">> := SecurityCode, <<"properties">> := #{<<"new_password">> := NewPassword, <<"new_salt">> := NewSalt}}} ->
            {ok, SecurityCode, NewPassword, NewSalt};
        {error, _Reason} = Error ->
            Error
    end.

-spec set_password_reset_confirm(ClientId :: non_neg_integer(), SecurityCode :: binary(), NewPassword :: binary(), NewSalt :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

set_password_reset_confirm(ClientId, SecurityCode, NewPassword, NewSalt) ->
    {ok, Lifetime} = db_if_settings:password_reset_confirm_code_lifetime(),
    set(ClientId, ?cr_password_reset_confirm, SecurityCode, #{new_password => NewPassword, new_salt => NewSalt}, Lifetime).

-spec delete_password_reset_confirm(ClientId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

delete_password_reset_confirm(ClientId) ->
    delete(ClientId, ?cr_password_reset_confirm).

%% Local functions

common_entity_fields() ->
    % Aliases:
    % clir : client_requests
    % cli : clients
    <<
        "clir.client_id AS client_id, ",
        "cli.username AS username, ",
        "clir.request AS request, ",
        "clir.security_code AS security_code, ",
        "clir.properties AS properties, ",
        "clir.created_at AS created_at, ",
        "clir.valid_thru AS valid_thru "
    >>.

common_joins() ->
    <<
        "LEFT OUTER JOIN clients AS cli ON (clir.client_id = cli.id) "
    >>.
