-module(web_captcha).

-behaviour(gen_server).

%% Include files

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("aplib/include/apmacros.hrl").

%% Exported functions

-export([
    start_link/0,

    % Stored images
    get_one/1,
    add/2,
    add/3,
    delete/1,
    validate/2
]).

%% gen_server callbacks

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(refresh_interval, 60*1000).     % msec, 1 min
-define(captcha_lifetime, 10*60).       % sec, 10 min
-define(ets_captcha_images, web_captcha_images).

-record(state, {}).

-record(captcha_image, {
    captcha_key :: binary(),
    filename :: binary(),
    answer :: binary(),
    expires :: non_neg_integer()
}).

%% API

-spec start_link() ->
    {'ok', pid()} | 'ignore' | {'error', {'already_started', pid()} | term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_one(CaptchaKey :: binary()) ->
    {'ok', Answer :: binary(), FileName :: binary(), Expires :: non_neg_integer()} | 'undefined'.

get_one(CaptchaKey) ->
    case ets:lookup(?ets_captcha_images, CaptchaKey) of
        [#captcha_image{captcha_key = CaptchaKey, answer = Answer, filename = FileName, expires = Expires}|_] -> {ok, Answer, FileName, Expires};
        _ -> undefined
    end.

-spec add(FileName :: binary(), Answer :: binary()) -> binary().

add(FileName, Answer) ->
    Expires = util_time:utc_seconds() + ?captcha_lifetime,
    add(FileName, Answer, Expires).

-spec add(FileName :: binary(), Answer :: binary(), Expires :: non_neg_integer()) -> binary().

add(FileName, Answer, Expires) ->
    CaptchaJson = web_util:encode_json(#{answer => Answer, filename => FileName, expires => Expires}),
    CaptchaKey = jws:encode_compact(CaptchaJson, #{alg => <<"RS256">>}, web_storage:jwk()),
    ets:insert(?ets_captcha_images, #captcha_image{captcha_key = CaptchaKey, answer = Answer, filename = FileName, expires = Expires}),
    CaptchaKey.

-spec delete(CaptchaKey :: binary()) -> boolean().

delete(CaptchaKey) ->
    case get_one(CaptchaKey) of
        {ok, _Answer, FileName, _Expires} ->
            {ok, CaptchaDir} = web_config:captcha_dir(),
            file:delete(filename:join([CaptchaDir, FileName])),
            ets:delete(?ets_captcha_images, CaptchaKey);
        undefined -> false
    end.

-spec validate(CaptchaKey :: binary(), Answer :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

validate(CaptchaKey, Answer) ->
    Now = util_time:utc_seconds(),
    case get_one(CaptchaKey) of
        {ok, _RealAnswer, _FileName, Expires} when Expires < Now -> {error, captcha_expired};
        {ok, RealAnswer, _FileName, _Expires} when Answer =/= RealAnswer -> {error, invalid_captcha_response};
        {ok, _RealAnswer, _FileName, _Expires} -> ok;
        undefined -> {error, invalid_captcha_key}
    end.

%% gen_server callbacks
init(_Args) ->
    % Delete existing image files
    {ok, CaptchaDir} = web_config:captcha_dir(),
    Files = [util_binary:to_binary(FileName) || FileName <- filelib:wildcard("*.*", util_lists:to_list(CaptchaDir))],
    [file:delete(filename:join([CaptchaDir, FileName])) || FileName <- Files, FileName =/= <<".gitignore">>],

    % Create public named ETS tables
    ets:new(?ets_captcha_images, [set, public, named_table, {keypos, #captcha_image.captcha_key}]),
    erlang:send_after(?refresh_interval, self(), refresh),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    logger:debug("unhandled call ~p from ~p~n", [Request, From], #{caption => ?MODULE}),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    logger:debug("unhandled cast ~p~n", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

handle_info(refresh, State) ->
    % Get captcha directory and current timestamp
    {ok, CaptchaDir} = web_config:captcha_dir(),
    Now = util_time:utc_seconds(),

    % Define match spec for select, get outdated files and remove them
    MatchSpecSelect = ets:fun2ms(fun(#captcha_image{expires = Expires} = CI) when Expires < Now -> CI end),
    FilesToDelete = ets:select(?ets_captcha_images, MatchSpecSelect),
    [file:delete(filename:join([CaptchaDir, FileName])) || #captcha_image{filename = FileName} <- FilesToDelete],

    % Define match spec and delete outdated storage file entries
    MatchSpecDelete = ets:fun2ms(fun(#captcha_image{expires = Expires}) when Expires < Now -> true end),
    ets:select_delete(?ets_captcha_images, MatchSpecDelete),

    erlang:send_after(?refresh_interval, self(), refresh),
    {noreply, State};

handle_info(Msg, State) ->
    logger:debug("unhandled info ~p~n", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local functions
