-module(sendgrid_config).

%% Exported functions

-export([
    api_host/0,
    api_key/0,
    from_email/0,
    from_name/0,
    register_template_id/0,
    register_confirm_template_id/0,
    password_reset_template_id/0
]).

-define(sendgrid_app, sendgrid).

%% API

-spec api_host() -> binary().

api_host() ->
    {ok, ApiHost} = application:get_env(?sendgrid_app, api_host),
    util_binary:to_binary(ApiHost).

-spec api_key() -> binary().

api_key() ->
    {ok, ApiKey} = application:get_env(?sendgrid_app, api_key),
    util_binary:to_binary(ApiKey).

-spec from_email() -> binary().

from_email() ->
    {ok, From} = application:get_env(?sendgrid_app, from_email),
    util_binary:to_binary(From).

-spec from_name() -> binary().

from_name() ->
    {ok, From} = application:get_env(?sendgrid_app, from_name),
    util_binary:to_binary(From).

-spec register_template_id() -> binary().

register_template_id() ->
    {ok, TemplateId} = application:get_env(?sendgrid_app, register_template_id),
    util_binary:to_binary(TemplateId).

-spec register_confirm_template_id() -> binary().

register_confirm_template_id() ->
    {ok, TemplateId} = application:get_env(?sendgrid_app, register_confirm_template_id),
    util_binary:to_binary(TemplateId).

-spec password_reset_template_id() -> binary().

password_reset_template_id() ->
    {ok, TemplateId} = application:get_env(?sendgrid_app, password_reset_template_id),
    util_binary:to_binary(TemplateId).

%% Local functions
