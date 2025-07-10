-module(stripe).

%% Exported functions

-export([
    default_headers/0
]).

%% API

-spec default_headers() -> proplists:proplist().

default_headers() ->
    SecretKey = stripe_config:secret_key(),
    [{"Authorization", "Bearer " ++ SecretKey}].

%% Local functions
