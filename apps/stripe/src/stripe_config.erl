-module(stripe_config).

%% Exported functions

-export([
    secret_key/0,
    publishable_key/0
]).

-define(stripe_app, stripe).

%% API

-spec secret_key() -> string().

secret_key() ->
    {ok, SecretKey} = application:get_env(?stripe_app, secret_key),
    SecretKey.

-spec publishable_key() -> string().

publishable_key() ->
    {ok, PublishableKey} = application:get_env(?stripe_app, publishable_key),
    PublishableKey.

%% Local functions
