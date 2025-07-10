-module(db_if_game_ownership).

%% Include files

-include("protocol.hrl").

%% Exported functions

-export([
    get_one/2,
    get_for_game/5,
    get_for_game_count/1,
    get_for_client/5,
    get_for_client_count/1,
    get_for_client_all/1,
    create/5,
    delete/2,
    has_access/2
]).

%% API

-spec get_one(GameId :: non_neg_integer(), ClientId :: non_neg_integer()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one(GameId, ClientId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_ownership AS gmo ",
        (common_joins())/binary,
        "WHERE gmo.game_id = $1 AND gmo.client_id = $2 "
    >>,
    case db_query:select_one(Query, [GameId, ClientId]) of
        {ok, Item} -> {ok, Item};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_for_game(GameId :: non_neg_integer(), OrderBy :: binary(), OrderDir :: binary(), Offset :: non_neg_integer(), Limit :: non_neg_integer()) ->
    {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_for_game(GameId, OrderBy, OrderDir, Offset, Limit) ->
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Statement = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_ownership AS gmo ",
        (common_joins())/binary,
        "WHERE gmo.game_id = $1 ",
        Filter/binary
    >>,
    db_query:select(Statement, [GameId]).

-spec get_for_game_count(GameId :: non_neg_integer()) ->
    {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_for_game_count(GameId) ->
    Statement = <<"SELECT count(*)::bigint AS count FROM game_ownership WHERE game_id = $1">>,
    case db_query:select_one(Statement, [GameId]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_for_client(ClientId :: non_neg_integer(), OrderBy :: binary(), OrderDir :: binary(), Offset :: non_neg_integer(), Limit :: non_neg_integer()) ->
    {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_for_client(ClientId, OrderBy, OrderDir, Offset, Limit) ->
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Statement = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_ownership AS gmo ",
        (common_joins())/binary,
        "WHERE gmo.client_id = $1 ",
        Filter/binary
    >>,
    db_query:select(Statement, [ClientId]).

-spec get_for_client_count(ClientId :: non_neg_integer()) ->
    {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_for_client_count(ClientId) ->
    Statement = <<"SELECT count(*)::bigint AS count FROM game_ownership WHERE client_id = $1">>,
    case db_query:select_one(Statement, [ClientId]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_for_client_all(ClientId :: non_neg_integer()) ->
    {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_for_client_all(ClientId) ->
    Statement = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_ownership AS gmo ",
        (common_joins())/binary,
        "WHERE gmo.client_id = $1 "
    >>,
    db_query:select(Statement, [ClientId]).

-spec create(GameId, ClientId, Ownership, Properties, ValidThru) -> Result when
    GameId :: non_neg_integer(),
    ClientId :: non_neg_integer(),
    Ownership :: binary(),
    Properties :: jsx:json_term(),
    ValidThru :: calendar:datretime() | 'null',
    Result :: 'ok' | {'error', Reason :: atom()}.

create(GameId, ClientId, Ownership, Properties, ValidThru) ->
    Query = <<
        "INSERT INTO game_ownership (game_id, client_id, ownership, properties, valid_thru) ",
        "VALUES ($1, $2, $3, $4, $5) "
    >>,
    Params = [GameId, ClientId, Ownership, jsx:encode(Properties), ValidThru],
    case db_query:insert(Query, Params) of
        {ok, 1} -> ok;
        {error, unique_violation} -> {error, ?err_already_exists};
        {error, Other} -> {error, Other}
    end.

-spec delete(GameId :: non_neg_integer(), ClientId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

delete(GameId, ClientId) ->
    Query = <<"DELETE FROM game_ownership WHERE game_id = $1 AND client_id = $2">>,
    case db_query:delete(Query, [GameId, ClientId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec has_access(GameId :: non_neg_integer(), ClientId :: non_neg_integer()) ->
    {'ok', boolean()} | {'error', Reason :: atom()}.

has_access(GameId, ClientId) ->
    Statement = <<
        "SELECT count(*)::bigint AS count FROM game_ownership ",
        "WHERE game_id = $1 AND client_id = $2 AND ownership <> 'none' AND (valid_thru IS NULL OR valid_thru > NOW())"
    >>,
    case db_query:select_one(Statement, [GameId, ClientId]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count > 0};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % gmo : game_ownership
    <<
        "gmo.game_id AS game_id, ",
        "gmo.client_id AS client_id, ",
        "gmo.ownership AS ownership, ",
        "gmo.properties AS properties, ",
        "gmo.valid_thru AS valid_thru, ",
        "gmo.created_at AS created_at, ",
        "gmo.updated_at AS updated_at "
    >>.

common_joins() ->
    <<>>.
