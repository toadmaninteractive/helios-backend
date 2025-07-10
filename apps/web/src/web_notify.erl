-module(web_notify).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("settings.hrl").
-include("notification_protocol.hrl").
-include("web_protocol.hrl").

%% Exported functions

-export([
    game_category_created/1,
    game_category_updated/1,
    game_category_deleted/1,
    game_item_updated/1,
    game_item_updated/2
]).

%% API

-spec game_category_created(GameCategoryExcerpt) -> Result when
    GameCategoryExcerpt :: web_protocol:game_category_excerpt(),
    Result :: any().

game_category_created(GameCategoryExcerpt) ->
    proc_lib:spawn(fun() -> web_ws_personnel:broadcast(#game_category_created{data = GameCategoryExcerpt}) end).

-spec game_category_updated(GameCategoryExcerpt) -> Result when
    GameCategoryExcerpt :: web_protocol:game_category_excerpt(),
    Result :: any().

game_category_updated(GameCategoryExcerpt) ->
    proc_lib:spawn(fun() -> web_ws_personnel:broadcast(#game_category_updated{data = GameCategoryExcerpt}) end).

-spec game_category_deleted(GameCategoryExcerpt) -> Result when
    GameCategoryExcerpt :: web_protocol:game_category_excerpt(),
    Result :: any().

game_category_deleted(GameCategoryExcerpt) ->
    proc_lib:spawn(fun() -> web_ws_personnel:broadcast(#game_category_deleted{data = GameCategoryExcerpt}) end).

-spec game_item_updated(Guid) -> Result when
    Guid :: binary(),
    Result :: any().

game_item_updated(Guid) ->
    game_item_updated(Guid, undefined).

-spec game_item_updated(Guid, PersonnelIds) -> Result when
    Guid :: binary(),
    PersonnelIds :: [non_neg_integer()] | 'undefined',
    Result :: any().

game_item_updated(Guid, PersonnelIds) ->
    proc_lib:spawn(fun() ->
        {ok, #{<<"is_disabled">> := IsDisabled, <<"is_deleted">> := IsDeleted} = Game} = db_if_games:get_one(Guid),
        ?doif(not IsDisabled andalso not IsDeleted, begin
            PersonnelIds1 = ?yesno(is_list(PersonnelIds), PersonnelIds, begin
                {ok, Ids} = db_if_personnel:get_consumer_ids(Guid, windows),
                Ids
            end),
            GameItem = web_helper:to_game_item(Game),
            {ok, Branches} = db_if_game_branches:get_all(Guid),
            [begin
                AvailableBranches = accessible_branches(PersonnelId, Guid, Branches),
                Notification = #game_item_updated{data = GameItem#game_item{branches = AvailableBranches, ownership = employee}},
                ?yesno(AvailableBranches =/= [], web_ws_personnel:whisper(PersonnelId, Notification), ignore)
            end || PersonnelId <- PersonnelIds1]
        end)
    end).

%% Local functions

accessible_branches(PersonnelId, Guid, Branches) ->
    {ok, IsSuperadmin} = db_if_personnel:is_superadmin(PersonnelId),
    {ok, AccessData} = db_if_personnel_roles:get_one(PersonnelId, Guid),
    AvailableBranches = lists:filter(fun
        (#{<<"build_rev">> := ?null}) -> false;
        (#{<<"platform">> := Platform}) when Platform =/= <<"windows">> -> false;
        (#{<<"is_deleted">> := true}) -> false;
        (_) when IsSuperadmin -> true;
        (#{<<"id">> := BranchId}) -> web_helper:is_branch_available(BranchId, AccessData)
    end, Branches),
    [web_helper:to_game_branch_item(B) || B <- AvailableBranches].
