-module(db_if_game_category_assignments).

%% Include files

-include("protocol.hrl").

%% Exported functions

-export([
    get_for_game/1,
    get_for_category/1,
    create/2,
    delete/2,
    exists/2
]).

%% API

-spec get_for_game(GameId :: binary()) ->
    {'ok', [CategoryId :: non_neg_integer()]} | {'error', Reason :: atom()}.

get_for_game(GameId) ->
    Query = <<"SELECT category_id FROM game_category_assignments WHERE game_id = TRIM($1)">>,
    case db_query:select(Query, [GameId]) of
        {ok, Items} -> {ok, [CategoryId || #{<<"category_id">> := CategoryId} <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec get_for_category(CategoryId :: non_neg_integer()) ->
    {'ok', [GameId :: binary()]} | {'error', Reason :: atom()}.

get_for_category(CategoryId) ->
    Query = <<"SELECT game_id FROM game_category_assignments WHERE category_id = $1">>,
    case db_query:select(Query, [CategoryId]) of
        {ok, Items} -> {ok, [GameId || #{<<"game_id">> := GameId} <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec create(GameId :: binary(), CategoryId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

create(GameId, CategoryId) ->
    Query = <<
        "INSERT INTO game_category_assignments (game_id, category_id) ",
        "VALUES (TRIM($1), $2) ",
        "ON CONFLICT (game_id, category_id) DO UPDATE SET assigned_at = current_timestamp"
    >>,
    Params = [GameId, CategoryId],
    case db_query:insert(Query, Params) of
        {ok, _} -> ok;
        {error, Other} -> {error, Other}
    end.

-spec delete(GameId :: binary(), CategoryId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

delete(GameId, CategoryId) ->
    Query = <<"DELETE FROM game_category_assignments WHERE game_id = TRIM($1) AND category_id = $2">>,
    Params = [GameId, CategoryId],
    case db_query:delete(Query, Params) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec exists(GameId :: binary(), CategoryId :: non_neg_integer()) ->
    {'ok', boolean()} | {'error', Reason :: atom()}.

exists(GameId, CategoryId) ->
    Statement = <<
        "SELECT count(*)::bigint > 0 AS result ",
        "FROM game_category_assignments ",
        "WHERE game_id = TRIM($1) AND category_id = $2"
    >>,
    Params = [GameId, CategoryId],
    case db_query:select_one(Statement, Params) of
        {ok, #{<<"result">> := Result}} -> {ok, Result};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions
