-module(web_rest_callback_admin_game).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("web_protocol.hrl").
-include("session.hrl").
-include("limits.hrl").

%% Exported functions

-export([
    get_game/2,
    get_games/6,
    create_game/2,
    update_game/4,
    get_favourite_games/1,
    is_favourite_game/2,
    favour_game/3,
    unfavour_game/2,
    assign_game_category/4,
    unassign_game_category/3
]).

%% API

-spec get_game(Guid, Req) -> Response when
    Guid :: binary(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:game(), cowboy_req:req()}.

get_game(Guid, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, Game} = db_if_games:get_one(Guid),
    {web_protocol:game_from_json(Game), Req};

get_game(_Guid, _Req) ->
    % Unauthorized
    web_rest_admin_game:get_game_403(#forbidden_error{}).

-spec get_games(Needle, OrderBy, OrderDir, Offset, Limit, Req) -> Response when
    Needle :: binary() | 'undefined',
    OrderBy :: web_protocol:game_order_by(),
    OrderDir :: web_protocol:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:collection_slice(web_protocol:game()), cowboy_req:req()}.

get_games(Needle, OrderBy, OrderDir, Offset, Limit, #{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    {ok, Games} = db_if_games:get(UserId, Needle, util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit)),
    {ok, Total} = db_if_games:get_count(UserId, Needle),
    Games1 = lists:map(fun web_protocol:game_from_json/1, Games),
    {#collection_slice{items = Games1, total = Total}, Req};

get_games(_Needle, _OrderBy, _OrderDir, _Offset, _Limit, _Req) ->
    % Unauthorized
    web_rest_admin_games:get_games_403(#forbidden_error{}).

-spec create_game(Request, Req) -> Response when
    Request :: web_protocol:game_create_request(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:game_create_response(), cowboy_req:req()}.

create_game(Request, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    #game_create_request{id = Guid, title = Title, price = Price, currency = Currency} = Request,
    IsValidCurrency = lists:member(Currency, [<<"EUR">>, <<"USD">>, <<"RUB">>]),
    Result = if
        Price =< 0 -> #game_create_response{result = false, error = invalid_price};
        not IsValidCurrency -> #game_create_response{result = false, error = invalid_currency};
        true ->
            case db_if_games:create(Guid, Title, Price, Currency) of
                {ok, Guid} ->
                    {ok, Game} = db_if_games:get_one(Guid),
                    #game_create_response{result = true, game = web_protocol:game_from_json(Game)};
                {error, game_id_already_exists = Error} ->
                    #game_create_response{result = false, error = Error};
                {error, game_title_already_exists = Error} ->
                    #game_create_response{result = false, error = Error};
                {error, _Error} ->
                    #game_create_response{result = false, error = failure}
            end
    end,
    {Result, Req};

create_game(_Request, _Req) ->
    % Unauthorized
    web_rest_admin_games:create_game_403(#forbidden_error{}).

-spec update_game(Request, Guid, Rev, Req) -> Response when
    Request :: web_protocol:game_update_request(),
    Guid :: binary(),
    Rev :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:game_update_response(), cowboy_req:req()}.

update_game(Request, Guid, Rev, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    Patch = web_protocol:game_update_request_to_json(Request),
    NullableFields = [<<"jira_key">>, <<"selene_key">>, <<"ci_url">>, <<"discord_url">>],
    FilterAbsentFun = fun(_K, V) -> V =/= ?null end,
    EnsureNullsFun = fun(K, V) -> ?yesno(lists:member(K, NullableFields) andalso V =:= <<>>, ?null, V) end,
    Patch1 = maps:filter(FilterAbsentFun, Patch),
    Patch2 = maps:map(EnsureNullsFun, Patch1),
    FieldChecks = lists:map(fun
        ({<<"price">>, V}) -> ?yesno(V >= 0, ok, invalid_price);
        ({<<"currency">>, V}) -> ?yesno(lists:member(V, [<<"EUR">>, <<"USD">>, <<"RUB">>]), ok, invalid_currency);
        ({<<"jira_key">>, V}) when is_binary(V) -> ?yesno(util_validate:jira_key(V), ok, invalid_jira_key);
        ({<<"selene_key">>, V}) when is_binary(V) -> ?yesno(util_validate:selene_key(V), ok, invalid_selene_key);
        ({<<"ci_url">>, V}) when is_binary(V) -> ?yesno(util_validate:url(V), ok, invalid_ci_url);
        ({<<"discord_url">>, V}) when is_binary(V) -> ?yesno(util_validate:url(V), ok, invalid_discord_url);
        (_) -> ok
    end, maps:to_list(Patch2)),
    Errors = [R || R <- FieldChecks, R =/= ok],
    Response = if
        Errors =/= [] ->
            #game_update_response{result = false, error = hd(Errors)};
        map_size(Patch2) =< 0 ->
            #game_update_response{result = false, error = nothing_to_update};
        true ->
            UpdateResult = db_if_games:update(Guid, Rev, Patch2),
            {ok, Game} = db_if_games:get_one(Guid),
            GameIgor = web_protocol:game_from_json(Game),
            case UpdateResult of
                ok ->
                    web_notify:game_item_updated(Guid),
                    #game_update_response{result = true, game = GameIgor};
                {error, ?err_not_exists} ->
                    #game_update_response{result = false, error = rev_mismatch, game = GameIgor};
                {error, game_title_already_exists} ->
                    #game_update_response{result = false, error = game_title_already_exists};
                {error, invalid_game_title} ->
                    #game_update_response{result = false, error = invalid_game_title};
                {error, invalid_build_lifetime} ->
                    #game_update_response{result = false, error = invalid_build_lifetime};
                {error, _} ->
                    #game_update_response{result = false, error = failure}
            end
    end,
    {Response, Req};

update_game(_Request, _Guid, _Rev, _Req) ->
    % Unauthorized
    web_rest_admin_game_update:update_game_403(#forbidden_error{}).

-spec get_favourite_games(Req) -> Response when
    Req :: cowboy_req:req(),
    Response :: {web_protocol:collection(web_protocol:game()), cowboy_req:req()}.

get_favourite_games(#{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    case db_if_games:get_favourites(UserId, maintainer) of
        {ok, Games} -> {#collection{items = lists:map(fun web_protocol:game_from_json/1, Games)}, Req};
        {error, Reason} -> web_rest_admin_games_favourite:get_favourite_games_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end;

get_favourite_games(_Req) ->
    % Unauthorized
    web_rest_admin_games_favourite:get_favourite_games_403(#forbidden_error{}).

-spec is_favourite_game(Guid, Req) -> Response when
    Guid :: binary(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:generic_response(), cowboy_req:req()}.

is_favourite_game(Guid, #{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    case db_if_games:is_favourite(Guid, UserId) of
        {ok, Result} -> {#generic_response{result = Result}, Req};
        {error, Reason} -> web_rest_admin_game_favourite:is_favourite_game_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end;

is_favourite_game(_Guid, _Req) ->
    % Unauthorized
    web_rest_admin_game_favourite:is_favourite_game_403(#forbidden_error{}).

-spec favour_game(Empty, Guid, Req) -> Response when
    Empty :: web_protocol:empty(),
    Guid :: binary(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:generic_response(), cowboy_req:req()}.

favour_game(#empty{}, Guid, #{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    case db_if_games:exists(Guid) of
        {ok, true} ->
            case db_if_games:set_favourite(Guid, UserId) of
                ok -> {#generic_response{result = true}, Req};
                {error, Reason} -> web_rest_admin_game_favourite:favour_game_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {ok, false} ->
            web_rest_admin_game_favourite:favour_game_404(#not_found_error{});
        {error, Reason} ->
            web_rest_admin_game_favourite:favour_game_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end;

favour_game(_Empty, _Guid, _Req) ->
    % Unauthorized
    web_rest_admin_game_favourite:favour_game_403(#forbidden_error{}).

-spec unfavour_game(Guid, Req) -> Response when
    Guid :: binary(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:generic_response(), cowboy_req:req()}.

unfavour_game(Guid, #{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    case db_if_games:exists(Guid) of
        {ok, true} ->
            case db_if_games:unset_favourite(Guid, UserId) of
                ok -> {#generic_response{result = true}, Req};
                {error, Reason} -> web_rest_admin_game_favourite:unfavour_game_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {ok, false} ->
            web_rest_admin_game_favourite:unfavour_game_404(#not_found_error{});
        {error, Reason} ->
            web_rest_admin_game_favourite:unfavour_game_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end;

unfavour_game(_Guid, _Req) ->
    % Unauthorized
    web_rest_admin_game_favourite:unfavour_game_403(#forbidden_error{}).

-spec assign_game_category(Empty, Guid, CategoryId, Req) -> Response when
    Empty :: web_protocol:empty(),
    Guid :: binary(),
    CategoryId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:generic_response(), cowboy_req:req()}.

assign_game_category(#empty{}, Guid, CategoryId, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    case db_if_games:exists(Guid) of
        {ok, true} ->
            case db_if_game_category_assignments:create(Guid, CategoryId) of
                ok ->
                    web_notify:game_item_updated(Guid),
                    {#generic_response{result = true}, Req};
                {error, Reason} ->
                    web_rest_admin_game_category_manage:assign_game_category_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {ok, false} ->
            web_rest_admin_game_category_manage:assign_game_category_404(#not_found_error{});
        {error, Reason} ->
            web_rest_admin_game_category_manage:assign_game_category_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end;

assign_game_category(_Empty, _Guid, _CategoryId, _Req) ->
    % Unauthorized
    web_rest_admin_game_category_manage:assign_game_category_403(#forbidden_error{}).

-spec unassign_game_category(Guid, CategoryId, Req) -> Response when
    Guid :: binary(),
    CategoryId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:generic_response(), cowboy_req:req()}.

unassign_game_category(Guid, CategoryId, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    case db_if_games:exists(Guid) of
        {ok, true} ->
            case db_if_game_category_assignments:delete(Guid, CategoryId) of
                ok ->
                    web_notify:game_item_updated(Guid),
                    {#generic_response{result = true}, Req};
                {error, ?err_not_exists} ->
                    web_rest_admin_game_category_manage:unassign_game_category_404(#not_found_error{});
                {error, Reason} ->
                    web_rest_admin_game_category_manage:unassign_game_category_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {ok, false} ->
            web_rest_admin_game_category_manage:unassign_game_category_404(#not_found_error{});
        {error, Reason} ->
            web_rest_admin_game_category_manage:unassign_game_category_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end;

unassign_game_category(_Guid, _CategoryId, _Req) ->
    % Unauthorized
    web_rest_admin_game_category_manage:unassign_game_category_403(#forbidden_error{}).

%% Local functions
