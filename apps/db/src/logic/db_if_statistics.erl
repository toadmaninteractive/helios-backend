-module(db_if_statistics).

%% Include files

-include("protocol.hrl").

%% Exported functions

-export([
    get_most_popular_games/4,
    get_most_popular_games_count/0
]).

%% API

-spec get_most_popular_games(OrderBy :: binary(), OrderDir :: binary(), Offset :: non_neg_integer(), Limit :: non_neg_integer()) ->
    {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_most_popular_games(OrderBy, OrderDir, Offset, Limit) ->
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Query = <<
        "SELECT ",
            "gm.id AS id, ",
            "gm.title AS title, ",
            "gm.description AS description, ",
            "gm.price AS price, ",
            "gm.currency AS currency, ",
            "COUNT(DISTINCT gmo.client_id)::integer AS purchases ",
        "FROM game_ownership AS gmo ",
        "LEFT OUTER JOIN games AS gm ON (gmo.game_id = gm.id) ",
        "WHERE gmo.ownership = 'purchase' ",
        "GROUP BY (gm.id) ",
        Filter/binary
    >>,
    db_query:select(Query, []).

-spec get_most_popular_games_count() ->
    {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_most_popular_games_count() ->
    Query = <<"SELECT COUNT(DISTINCT game_id)::integer AS count FROM game_ownership WHERE ownership = 'purchase'">>,
    case db_query:select_one(Query, []) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions
