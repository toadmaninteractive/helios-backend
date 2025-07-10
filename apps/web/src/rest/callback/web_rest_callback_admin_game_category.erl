-module(web_rest_callback_admin_game_category).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("web_protocol.hrl").
-include("session.hrl").
-include("limits.hrl").

%% Exported functions

-export([
    get_game_category/2,
    get_game_categories/1,
    create_game_category/2,
    update_game_category/4,
    delete_game_category/2
]).

%% API

-spec get_game_category(Id, Req) -> Response when
    Id :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:game_category(), cowboy_req:req()}.

get_game_category(Id, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, Category} = db_if_game_categories:get_one(Id),
    {web_protocol:game_category_from_json(Category), Req};

get_game_category(_Id, _Req) ->
    % Unauthorized
    web_rest_admin_game_category:get_game_category_403(#forbidden_error{}).

-spec get_game_categories(Req) -> Response when
    Req :: cowboy_req:req(),
    Response :: {web_protocol:collection(web_protocol:game_category()), cowboy_req:req()}.

get_game_categories(#{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, Categories} = db_if_game_categories:get_all(),
    Categories1 = lists:map(fun web_protocol:game_category_from_json/1, Categories),
    {#collection{items = Categories1}, Req};

get_game_categories(_Req) ->
    % Unauthorized
    web_rest_admin_game_categories:get_game_categories_403(#forbidden_error{}).

-spec create_game_category(Request, Req) -> Response when
    Request :: web_protocol:game_category_create_request(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:game_category_manage_response(), cowboy_req:req()}.

create_game_category(Request, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    #game_category_create_request{name = Name, description = Description, sort_order = SortOrder} = Request,
    Result = case db_if_game_categories:create(Name, Description, SortOrder) of
        {ok, Id} ->
            {ok, CategoryJson} = db_if_game_categories:get_one(Id),

            % Notify of created game category
            GameCategoryExcerpt = web_protocol:game_category_excerpt_from_json(CategoryJson),
            web_notify:game_category_created(GameCategoryExcerpt),

            % Reply
            GameCategory = web_protocol:game_category_from_json(CategoryJson),
            #game_category_manage_response{result = true, category = GameCategory};
        {error, ?err_already_exists = Error} ->
            #game_category_manage_response{result = false, error = Error};
        {error, invalid_name = Error} ->
            #game_category_manage_response{result = false, error = Error};
        {error, _Error} ->
            #game_category_manage_response{result = false, error = failure}
    end,
    {Result, Req};

create_game_category(_Request, _Req) ->
    % Unauthorized
    web_rest_admin_game_categories:create_game_category_403(#forbidden_error{}).

-spec update_game_category(Request, Id, Rev, Req) -> Response when
    Request :: web_protocol:game_update_request(),
    Id :: non_neg_integer(),
    Rev :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:game_category_manage_response(), cowboy_req:req()}.

update_game_category(Request, Id, Rev, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    Patch = web_protocol:game_category_update_request_to_json(Request),
    FilterAbsentFun = fun(_K, V) -> V =/= ?null end,
    Patch1 = maps:filter(FilterAbsentFun, Patch),
    Response = case db_if_game_categories:update(Id, Rev, Patch1) of
        ok ->
            {ok, CategoryJson} = db_if_game_categories:get_one(Id),

            % Notify of created game category
            GameCategoryExcerpt = web_protocol:game_category_excerpt_from_json(CategoryJson),
            web_notify:game_category_updated(GameCategoryExcerpt),

            % Reply
            GameCategory = web_protocol:game_category_from_json(CategoryJson),
            #game_category_manage_response{result = true, category = GameCategory};
        {error, ?err_already_exists = Error} ->
            #game_category_manage_response{result = false, error = Error};
        {error, invalid_name = Error} ->
            #game_category_manage_response{result = false, error = Error};
        {error, _Error} ->
            #game_category_manage_response{result = false, error = failure}
    end,
    {Response, Req};

update_game_category(_Request, _Guid, _Rev, _Req) ->
    % Unauthorized
    web_rest_admin_game_category_update:update_game_category_403(#forbidden_error{}).

-spec delete_game_category(Id, Req) -> Response when
    Id :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:generic_response(), cowboy_req:req()}.

delete_game_category(Id, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    case db_if_game_categories:get_one(Id) of
        {ok, GameCategoryJson} ->
            {ok, AffectedGuids} = db_if_game_category_assignments:get_for_category(Id),
            case db_if_game_categories:delete(Id) of
                ok ->
                    % Notify of updated games
                    lists:foreach(fun web_notify:game_item_updated/1, AffectedGuids),

                    % Notify of deleted game category
                    GameCategoryExcerpt = web_protocol:game_category_excerpt_from_json(GameCategoryJson),
                    web_notify:game_category_deleted(GameCategoryExcerpt),

                    {#generic_response{result = true}, Req};
                {error, _Reason} ->
                    {#generic_response{result = false}, Req}
            end;
        {error, _Reason} ->
            {#generic_response{result = false}, Req}
    end;

delete_game_category(_Id, _Req) ->
    % Unauthorized
    web_rest_admin_game_category:delete_game_category_403(#forbidden_error{}).

%% Local functions
