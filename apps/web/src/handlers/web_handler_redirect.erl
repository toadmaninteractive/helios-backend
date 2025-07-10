-module(web_handler_redirect).

-behaviour(cowboy_handler).

%% Exported functions

-export([
    init/2
]).

%% API

init(Req, Opts) ->
    Headers = #{<<"location">> => <<"/admin">>},
    Req1 = cowboy_req:reply(302, Headers, <<"302 Found">>, Req),
    {ok, Req1, Opts}.

%% Local functions
