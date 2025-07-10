-module(captcha_config).

%% Exported functions

-export([
    font/0
]).

-define(captcha_app, captcha).

%% API

-spec font() -> string().

font() ->
    {ok, Font} = application:get_env(?captcha_app, font),
    util_lists:to_list(Font).

%% Local functions
