-module(sendgrid).

%% Exported functions

-export([
    notify_register/3,
    notify_register_confirm/2,
    notify_password_reset/3
]).

%% API

-spec notify_register(ToEmail :: binary(), Username :: binary(), SecurityCode :: binary()) ->
    {'ok', MessageId :: binary()} | {'error', atom()}.

notify_register(ToEmail, Username, SecurityCode) ->
    TemplateId = sendgrid_config:register_template_id(),
    sendgrid_api:sendmail(ToEmail, Username, TemplateId, #{username => Username, activation_code => SecurityCode}).

-spec notify_register_confirm(ToEmail :: binary(), Username :: binary()) ->
    {'ok', MessageId :: binary()} | {'error', atom()}.

notify_register_confirm(ToEmail, Username) ->
    TemplateId = sendgrid_config:register_confirm_template_id(),
    sendgrid_api:sendmail(ToEmail, Username, TemplateId, #{username => Username}).

-spec notify_password_reset(ToEmail :: binary(), Username :: binary(), SecurityCode :: binary()) ->
    {'ok', MessageId :: binary()} | {'error', atom()}.

notify_password_reset(ToEmail, Username, SecurityCode) ->
    TemplateId = sendgrid_config:password_reset_template_id(),
    sendgrid_api:sendmail(ToEmail, Username, TemplateId, #{username => Username, reset_code => SecurityCode}).

%% Local functions
