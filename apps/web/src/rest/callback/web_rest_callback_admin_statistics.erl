-module(web_rest_callback_admin_statistics).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("web_protocol.hrl").
-include("session.hrl").
-include("limits.hrl").

%% Exported functions

-export([
    get_popular_games/5
]).

%% API

-spec get_popular_games(OrderBy, OrderDir, Offset, Limit, Req) -> Response when
    OrderBy :: web_protocol:popular_game_order_by(),
    OrderDir :: web_protocol:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:collection_slice(web_protocol:popular_game()), cowboy_req:req()}.

get_popular_games(OrderBy, OrderDir, Offset, Limit, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, PopularGames} = db_if_statistics:get_most_popular_games(util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit)),
    {ok, Total} = db_if_statistics:get_most_popular_games_count(),
    PopularGames1 = lists:map(fun web_protocol:popular_game_from_json/1, PopularGames),
    {#collection_slice{items = PopularGames1, total = Total}, Req};

get_popular_games(_OrderBy, _OrderDir, _Offset, _Limit, _Req) ->
    % Unauthorized
    web_rest_admin_popular_games:get_popular_games_403(#forbidden_error{}).

%% Local functions
