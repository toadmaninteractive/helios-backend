-module(db_if_client_sessions).

%% Include files

-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get/0,
    create/3,
    prolong/2,
    delete/1,
    delete_for/1,
    cleanup_expired/0
]).

%% API

-spec get_one(SessionId :: binary()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one(SessionId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM client_sessions AS clis ",
        (common_joins())/binary,
        "WHERE clis.id = $1"
    >>,
    case db_query:select_one(Query, [SessionId]) of
        {ok, Session} -> {ok, Session};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get() ->
    {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get() ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM client_sessions AS clis ",
        (common_joins())/binary
    >>,
    db_query:select(Query, []).

-spec create(SessionId :: binary(), ClientId :: non_neg_integer(), Lifetime :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

create(SessionId, ClientId, Lifetime) ->
    Query = <<
        "INSERT INTO client_sessions (id, client_id, valid_thru) ",
        "VALUES ($1, $2, NOW() + make_interval(secs => $3::integer))"
    >>,
    case db_query:insert(Query, [SessionId, ClientId, Lifetime]) of
        {ok, 1} -> ok;
        {error, unique_violation} -> {error, ?err_already_exists};
        {error, foreign_key_violation} -> {error, ?err_not_exists};
        {error, Other} -> {error, Other}
    end.

-spec prolong(SessionId :: binary(), Lifetime :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

prolong(SessionId, Lifetime) ->
    Query = <<"UPDATE client_sessions SET valid_thru = NOW() + make_interval(secs => $2::integer) WHERE id = $1">>,
    case db_query:update(Query, [SessionId, Lifetime]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec delete(SessionId :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

delete(SessionId) ->
    Query = <<"DELETE FROM client_sessions WHERE id = $1">>,
    case db_query:delete(Query, [SessionId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec delete_for(ClientId :: non_neg_integer()) ->
    {'ok', NumDeleted :: non_neg_integer()} | {'error', Reason :: atom()}.

delete_for(ClientId) ->
    Query = <<"DELETE FROM client_sessions WHERE client_id = $1">>,
    case db_query:delete(Query, [ClientId]) of
        {ok, NumDeleted} -> {ok, NumDeleted};
        {error, Reason} -> {error, Reason}
    end.

-spec cleanup_expired() ->
    'ok' | {'error', Reason :: atom()}.

cleanup_expired() ->
    Query = <<"DELETE FROM client_sessions WHERE valid_thru < NOW()">>,
    case db_query:delete(Query, []) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % clis : client_sessions
    % cli : clients
    <<
        "clis.id AS id, ",
        "clis.client_id AS client_id, ",
        "cli.username AS username, ",
        "clis.created_at AS created_at, ",
        "clis.valid_thru AS valid_thru "
    >>.

common_joins() ->
    <<
        "LEFT OUTER JOIN clients AS cli ON (clis.client_id = cli.id) "
    >>.
