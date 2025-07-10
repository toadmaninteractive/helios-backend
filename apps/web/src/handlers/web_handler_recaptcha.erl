-module(web_handler_recaptcha).

-behaviour(cowboy_handler).

%% Include files

-include_lib("db/include/protocol.hrl").

%% Exported functions

-export([
    init/2
]).

%% API

init(Req, Opts) ->
    {ok, SiteKey} = web_config:recaptcha_site_key(),
    FormHtml = web_util:read_form(captcha),
    ReplyBody = web_util:interpolate(FormHtml, #{site_key => SiteKey}),
    Req1 = cowboy_req:reply(200, #{}, ReplyBody, Req),
    {ok, Req1, Opts}.

%% Local functions
