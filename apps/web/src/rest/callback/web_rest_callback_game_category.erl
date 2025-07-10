-module(web_rest_callback_game_category).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("web_protocol.hrl").
-include("session.hrl").

%% Exported functions

-export([
    get_game_categories/1
]).

%% API

-spec get_game_categories(Req) -> Response when
    Req :: cowboy_req:req(),
    Response :: {web_protocol:game_category_excerpt_list(), cowboy_req:req()}.

get_game_categories(#{?m_session := #session{key = {_Actor, _}, user_id = _UserId}} = Req) ->
    % Client / Personnel
    {ok, Categories} = db_if_game_categories:get_all(),
    CategoryExcerpts = [web_protocol:game_category_excerpt_from_json(Json) || Json <- Categories],
    {#game_category_excerpt_list{categories = CategoryExcerpts}, Req};

get_game_categories(_Req) ->
    % Unauthorized
    web_rest_game_categories:get_game_categories_403(#forbidden_error{}).

%% Local functions
