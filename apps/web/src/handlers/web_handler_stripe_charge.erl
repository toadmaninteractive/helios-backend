-module(web_handler_stripe_charge).

-behaviour(cowboy_handler).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include_lib("stripe/include/stripe_protocol.hrl").

%% Exported functions

-export([
    init/2
]).

%% API

init(Req, Opts) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    #{<<"stripeEmail">> := Email, <<"stripeToken">> := StripeToken} = maps:from_list(cow_qs:parse_qs(Body)),
    #{<<"guid">> := Guid, <<"username">> := Username} = maps:from_list(cowboy_req:parse_qs(Req1)),
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
        {{ok, #{<<"id">> := Uid}}, {ok, #{<<"title">> := T, <<"price">> := P, <<"currency">> := C}}} ->
            case db_if_game_ownership:has_access(Guid, Uid) of
                {ok, true} -> {error, <<"Game already owned">>};
                {ok, false} -> {ok, Uid, T, P, C};
                {error, _} -> {error, <<"Failed to check game ownership">>}
            end;
        {_, _} -> {error, <<"Something is not right, try again later...">>}
    end,
    ErrorHtml = web_util:read_form(error),
    ReplyBody = case CheckResult of
        {error, Reason} ->
            web_util:interpolate(ErrorHtml, #{error => Reason});
        {ok, UserId, GameTitle, Price, Currency} ->
            % Create new customer
            CustomerRequest = #create_customer_request{
                email = Email,
                source = StripeToken,
                description = <<"Customer for: ", Email/binary>>
            },
            #customer{id = CustomerId} = stripe_api:create_customer(CustomerRequest),

            % Create charge and check its result
            ChargeRequest = #create_charge_request{
                amount = trunc(Price) * 100,
                currency = util_binary:to_lower(Currency),
                description = <<"Purchase ", GameTitle/binary>>,
                customer = CustomerId
            },
            case stripe_api:create_charge(ChargeRequest) of
                #charge{id = ChargeId, paid = true} ->
                    Properties = #{email => Email, stripe_customer => CustomerId, stripe_charge => ChargeId},
                    case db_if_game_ownership:create(Guid, UserId, <<"purchase">>, Properties, ?null) of
                        ok ->
                            web_util:read_form(success);
                        {error, Reason1} ->
                            Reason1A = util_binary:to_binary(Reason1),
                            web_util:interpolate(ErrorHtml, #{error => <<"Failed to register ownership, reason: ", Reason1A/binary>>})
                    end;
                _ ->
                    web_util:interpolate(ErrorHtml, #{error => <<"Failed to process payment">>})
            end
    end,
    Req2 = cowboy_req:reply(200, #{}, ReplyBody, Req1),
    {ok, Req2, Opts}.

%% Local functions
