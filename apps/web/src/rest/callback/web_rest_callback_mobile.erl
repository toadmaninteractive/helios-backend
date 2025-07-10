-module(web_rest_callback_mobile).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("web_protocol.hrl").
-include("session.hrl").

%% Exported functions

-export([
    get_mobile_games/2,
    get_mobile_builds/7
]).

%% API

-spec get_mobile_games(Platform, Req) -> Response when
    Platform :: web_protocol:platform(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:game_item_list(), cowboy_req:req()}.

get_mobile_games(Platform, #{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    {ok, Games} = db_if_games:get_accessible_for_platform(UserId, Platform),
    GameList = lists:foldr(fun(JsonItem, Acc) ->
        Game = web_helper:to_game_item(JsonItem),
        Ownership = employee,
        {ok, Branches} = db_if_game_branches:get_all_for_platform(Game#game_item.guid, Platform),
        AvailableBranches = lists:filter(fun
            (#{<<"build_rev">> := ?null}) -> false;
            (#{<<"is_deleted">> := true}) -> false;
            (_) -> true
        end, Branches),
        BranchList = [web_helper:to_game_branch_item(B) || B <- AvailableBranches],
        IsDisabled = maps:get(<<"is_disabled">>, JsonItem, false),
        IsDeleted = maps:get(<<"is_deleted">>, JsonItem, false),
        CanServeGame = BranchList =/= [] andalso not IsDisabled andalso not IsDeleted,
        ?yesno(CanServeGame, [Game#game_item{branches = BranchList, ownership = Ownership}|Acc], Acc)
    end, [], Games),
    {#game_item_list{games = GameList}, Req};

get_mobile_games(_Platform, _Req) ->
    % Unauthorized
    web_rest_mobile_games:get_mobile_games_403(#forbidden_error{}).

-spec get_mobile_builds(Guid, Platform, OrderBy, OrderDir, Offset, Limit, Req) -> Response when
    Guid :: binary(),
    OrderBy :: web_protocol:build_order_by(),
    OrderDir :: web_protocol:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Platform :: web_protocol:platform(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:game_item_list(), cowboy_req:req()}.

get_mobile_builds(Guid, Platform, OrderBy, OrderDir, Offset, Limit, #{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    {ok, IsGameAccessible} = db_if_games:is_accessible(Guid, UserId),
    OrderBy1 = web_protocol:build_order_by_to_json(OrderBy),
    OrderDir1 = web_protocol:order_direction_to_json(OrderDir),
    {ok, Builds} = db_if_game_builds:get_for_game_employee(Guid, Platform, UserId, IsGameAccessible, OrderBy1, OrderDir1, Offset, Limit),
    {ok, Total} = db_if_game_builds:get_for_game_employee_count(Guid, Platform, UserId, IsGameAccessible),
    Builds1 = lists:map(fun web_protocol:build_from_json/1, Builds),
    {#collection_slice{items = Builds1, total = Total}, Req};

get_mobile_builds(_Guid, _Platform, _OrderBy, _OrderDir, _Offset, _Limit, _Req) ->
    % Unauthorized
    web_rest_mobile_builds:get_mobile_builds_403(#forbidden_error{}).

%% Local functions
