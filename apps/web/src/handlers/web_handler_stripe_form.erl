-module(web_handler_stripe_form).

-behaviour(cowboy_handler).

%% Include files

-include_lib("db/include/protocol.hrl").

%% Exported functions

-export([
    init/2
]).

%% API

init(Req, Opts) ->
    QsVals = maps:from_list(cowboy_req:parse_qs(Req)),
    Guid = maps:get(<<"guid">>, QsVals, <<>>),
    Username = maps:get(<<"username">>, QsVals, <<>>),
    GData = db_if_games:get_one(Guid),
    UData = db_if_clients:get_one_by_username(Username),
    CheckResult = case {UData, GData} of
        {{error, ?err_not_exists}, _} -> {error, <<"Account not exists">>};
        {{ok, #{<<"is_activated">> := false}}, _} -> {error, <<"Account not activated">>};
        {{ok, #{<<"is_blocked">> := true}}, _} -> {error, <<"Account is blocked">>};
        {{ok, #{<<"is_deleted">> := true}}, _} -> {error, <<"Account is deleted">>};
        {_, {error, ?err_not_exists}} -> {error, <<"Game not exists">>};
        {_, {ok, #{<<"is_published">> := false}}} -> {error, <<"Game is not published yet">>};
        {_, {ok, #{<<"is_deleted">> := true}}} -> {error, <<"Game is deleted">>};
        {_, {ok, #{<<"price">> := P}}} when P < 0.01 -> {error, <<"Game price is not set correctly">>};
        {{ok, #{<<"id">> := Uid, <<"email">> := E}}, {ok, #{<<"title">> := T, <<"price">> := P, <<"currency">> := C}}} ->
            case db_if_game_ownership:has_access(Guid, Uid) of
                {ok, true} -> {error, <<"Game already owned">>};
                {ok, false} -> {ok, E, T, P, C};
                {error, _} -> {error, <<"Failed to check game ownership">>}
            end;
        {_, _} -> {error, <<"Something is not right, try again later...">>}
    end,
    ReplyBody = case CheckResult of
        {error, Reason} ->
            ErrorHtml = web_util:read_form(error),
            web_util:interpolate(ErrorHtml, #{error => Reason});
        {ok, Email, GameTitle, Price, Currency} ->
            FormHtml = web_util:read_form(advanced),
            web_util:interpolate(FormHtml, #{
                guid => Guid,
                username => Username,
                publishable_key => stripe_config:publishable_key(),
                amount => Price,
                email => Email,
                currency => Currency,
                game_title => GameTitle
            })
    end,
    Req1 = cowboy_req:reply(200, #{}, ReplyBody, Req),
    {ok, Req1, Opts}.

%% Local functions
