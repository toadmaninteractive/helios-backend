-module(web_rest_callback_captcha).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("web_protocol.hrl").
-include("session.hrl").

%% Exported functions

-export([
    request_captcha/1
]).

%% API

-spec request_captcha(Req) -> Response when
    Req :: cowboy_req:req(),
    Response :: {web_protocol:captcha_response(), cowboy_req:req()}.

request_captcha(Req) ->
    {ok, CaptchaDir} = web_config:captcha_dir(),
    Response = case captcha:generate(CaptchaDir) of
        {ok, Answer, FileName} -> #captcha_response{result = true, captcha_key = web_captcha:add(FileName, Answer), filename = FileName};
        {error, failure} -> #captcha_response{result = false, error = failure}
    end,
    {Response, Req}.

%% Local functions
