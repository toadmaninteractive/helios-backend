-module(server).

%% Exported functions

-export([
    start/0
]).

%% API

-spec start() ->
    {'ok', [atom()]} | {'error', Reason::term()}.

start() ->
    application:ensure_all_started(server).

%% Local functions
