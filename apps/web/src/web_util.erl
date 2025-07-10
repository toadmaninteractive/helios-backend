-module(web_util).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("http.hrl").
-include("settings.hrl").
-include("qs.hrl").

%% Exported functions

-export([
    encode_json/1,
    decode_json/1,
    decode_json_safe/2,
    reply_body/5,
    reply_json/4,
    reply_json_error/4,
    reply_json_error/5,
    reply_json_result/4,
    reply_json_explicit/3,
    json_body_partial/2,
    json_body_full/2,
    browser_supports_gzip/1,
    maybe_null/1,
    peer_ipv4/1,
    validate_recaptcha/1,
    validate_recaptcha/2,
    random_filename/1,
    random_filename/2,
    uuid32/0,
    uuid64/0,
    security_code/0,
    read_form/1,
    interpolate/2
]).

-define(max_body_length, 128 * 1024).   % 128 KB

%% API

-spec encode_json(Json :: jsx:json_term()) -> jsx:json_text().

encode_json(Json) ->
    jsx:encode(Json).

-spec decode_json(Binary :: jsx:json_text()) -> jsx:json_term().

decode_json(Binary) ->
    jsx:decode(Binary, [return_maps]).

-spec decode_json_safe(Binary :: jsx:json_text(), Default :: jsx:json_term()) -> jsx:json_term().

decode_json_safe(Binary, Default) ->
    try jsx:decode(Binary, [return_maps])
    catch _:_ -> Default
    end.

-spec reply_body(cowboy:http_status(), cowboy:http_headers(), Body :: iodata(), cowboy_req:req(), State :: term()) ->
    {'stop', cowboy_req:req(), term()}.

reply_body(Status, Headers, Body, Req, State) ->
    Req1 = cowboy_req:reply(Status, Headers, Body, Req),
    {stop, Req1, State}.

-spec reply_json(cowboy:http_status(), Json :: jsx:json_term(), cowboy_req:req(), State :: term()) ->
    {'stop', cowboy_req:req(), term()}.

reply_json(Status, Json, Req, State) ->
    Headers = #{<<"content-type">> => <<"application/json">>},
    reply_body(Status, Headers, encode_json(Json), Req, State).

-spec reply_json_error(cowboy:http_status(), Error :: atom() | binary(), cowboy_req:req(), State :: term()) ->
    {'stop', cowboy_req:req(), term()}.

reply_json_error(Status, Error, Req, State) ->
    reply_json_error(Status, Error, undefined, Req, State).

-spec reply_json_error(cowboy:http_status(), Error :: atom() | binary(), Fields :: maps:map() | 'undefined', cowboy_req:req(), State :: term()) ->
    {'stop', cowboy_req:req(), term()}.

reply_json_error(Status, Error, Fields, Req, State) ->
    Headers = #{<<"content-type">> => <<"application/json">>},
    ReplyJson = #{error => Error},
    ReplyJson1 = ?yesno(is_map(Fields) andalso map_size(Fields) > 0, ReplyJson#{fields => Fields}, ReplyJson),
    reply_body(Status, Headers, encode_json(ReplyJson1), Req, State).

-spec reply_json_result(cowboy:http_status(), Result :: boolean(), cowboy_req:req(), State :: term()) ->
    {'stop', cowboy_req:req(), term()}.

reply_json_result(Status, Result, Req, State) ->
    Headers = #{<<"content-type">> => <<"application/json">>},
    ReplyJson = #{result => Result},
    reply_body(Status, Headers, encode_json(ReplyJson), Req, State).

-spec reply_json_explicit(cowboy:http_status(), Json :: jsx:json_term(), cowboy_req:req()) ->
    cowboy_req:req().

reply_json_explicit(Status, Json, Req) ->
    Headers = #{<<"content-type">> => <<"application/json">>},
    Body = encode_json(Json),
    cowboy_req:reply(Status, Headers, Body, Req).

-spec json_body_partial(cowboy_req:req(), Default :: term()) -> Result when
    Result :: {jsx:json_term() | term(), cowboy_req:req()}.

json_body_partial(Req, Default) ->
    case cowboy_req:has_body(Req) of
        true ->
            case read_body_chunk(Req, ?max_body_length) of
                {ok, Body, Req1} -> {decode_json_safe(Body, Default), Req1};
                {error, Req1} -> {Default, Req1}
            end;
        false ->
            {Default, Req}
    end.

-spec json_body_full(cowboy_req:req(), Default :: term()) -> Result when
    Result :: {jsx:json_term() | term(), cowboy_req:req()}.

json_body_full(Req, Default) ->
    case cowboy_req:has_body(Req) of
        true ->
            case read_body(Req, <<>>) of
                {ok, Body, Req1} -> {decode_json_safe(Body, Default), Req1};
                {error, Req1} -> {Default, Req1}
            end;
        false ->
            {Default, Req}
    end.

-spec browser_supports_gzip(Req :: cowboy_req:req()) -> boolean().

browser_supports_gzip(Req) ->
    case cowboy_req:parse_header(<<"accept-encoding">>, Req) of
        Tokens when is_list(Tokens) -> lists:keyfind(<<"gzip">>, 1, Tokens) =/= false;
        _ -> false
    end.

-spec maybe_null(Value :: 'null' | jsx:json_term()) -> jsx:json_term().

maybe_null(null) -> undefined;
maybe_null(Value) -> Value.

-spec peer_ipv4(Req :: cowboy_req:req()) -> binary().

peer_ipv4(Req) ->
    {ok, ProxyEnabled} = web_config:proxy_enabled(),
    case ProxyEnabled of
        true ->
            cowboy_req:header(<<"x-forwarded-for">>, Req, <<"127.0.0.1">>);
        false ->
            {{A, B, C, D}, _} = cowboy_req:peer(Req),
            iolist_to_binary(io_lib:format("~B.~B.~B.~B", [A, B, C, D]))
    end.

-spec validate_recaptcha(RecaptchaResponse :: binary()) -> boolean().

validate_recaptcha(RecaptchaResponse) ->
    {ok, SecretKey} = web_config:recaptcha_secret_key(),
    validate_recaptcha(RecaptchaResponse, SecretKey).

-spec validate_recaptcha(RecaptchaResponse :: binary(), SecretKey :: binary()) -> boolean().

validate_recaptcha(RecaptchaResponse, SecretKey) ->
    Url = io_lib:format("https://www.google.com/recaptcha/api/siteverify?secret=~s&response=~s", [SecretKey, RecaptchaResponse]),
    case ibrowse:send_req(Url, [], get) of
        {ok, "200", _Headers, Body} ->
            Response = decode_json_safe(iolist_to_binary(Body), #{}),
            maps:get(<<"success">>, Response, false) =:= true;
        _ -> false
    end.

-spec random_filename(Extension :: binary()) -> binary().

random_filename(Extension) ->
    random_filename(<<>>, Extension).

-spec random_filename(Prefix :: binary(), Extension :: binary()) -> binary().

random_filename(Prefix, Extension) ->
    Timestamp = integer_to_list(util_time:utc_milliseconds(), 16),
    Timestamp1 = util_binary:to_binary(string:to_lower(Timestamp)),
    Prefix1 = util_binary:to_binary(Prefix),
    Prefix2 = ?yesno(Prefix1 =:= <<>>, <<>>, <<Prefix1/binary, "_">>),
    RandomPart = util_hex:from_binary(crypto:strong_rand_bytes(16)),
    Extension1 = util_binary:to_binary(Extension),
    Extension2 = ?yesno(Extension1 =:= <<>>, <<>>, <<".", Extension1/binary>>),
    <<Prefix2/binary, Timestamp1/binary, "_", RandomPart/binary, Extension2/binary>>.

-spec uuid32() -> binary().

uuid32() ->
    util_hex:from_binary(uuid:get_v4(strong)).

-spec uuid64() -> binary().

uuid64() ->
    Uuid1 = util_hex:from_binary(uuid:get_v4(strong)),
    Uuid2 = util_hex:from_binary(uuid:get_v4(strong)),
    <<Uuid1/binary, Uuid2/binary>>.

-spec security_code() -> binary().

security_code() ->
    A = util_hex:from_binary(crypto:strong_rand_bytes(3)),
    util_binary:to_upper(A).

-spec read_form(Kind :: basic | advanced | error | success) -> binary().

read_form(basic) -> read_form(stripe, "basic.html");
read_form(advanced) -> read_form(stripe, "advanced.html");
read_form(error) -> read_form(stripe, "error.html");
read_form(success) -> read_form(stripe, "success.html");
read_form(captcha) -> read_form(validation, "captcha.html").

-spec interpolate(FormHtml :: binary(), Data :: maps:map()) -> binary().

interpolate(FormHtml, Data) when is_map(Data) ->
    maps:fold(fun(K, V, Acc) ->
        K1 = util_binary:to_binary(K),
        V1 = util_binary:to_binary(V),
        binary:replace(Acc, <<"{", K1/binary, "}">>, V1, [global])
    end, FormHtml, Data);
interpolate(FormHtml, _) -> FormHtml.

%% Local functions

read_body(Req, Acc) ->
    %% Read whole body
    case cowboy_req:read_body(Req) of
        {ok, Data, Req1} -> {ok, <<Acc/binary, Data/binary>>, Req1};
        {more, Data, Req1} -> read_body(Req1, <<Acc/binary, Data/binary>>)
    end.

read_body_chunk(Req, Length) ->
    %% Read body chunk
    case cowboy_req:read_body(Req, #{length => Length}) of
        {ok, Data, Req1} -> {ok, Data, Req1};
        {more, _Data, Req1} -> {error, too_long, Req1}
    end.

read_form(stripe, Filename) -> read_form_contents(filename:join([filename:absname(""), "web", "stripe", Filename]));
read_form(validation, Filename) -> read_form_contents(filename:join([filename:absname(""), "web", "validation", Filename])).

read_form_contents(FilePath) ->
    {ok, FormHtml} = file:read_file(FilePath),
    FormHtml.
