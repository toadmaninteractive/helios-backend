-module(web_rest_callback_admin_settings).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("web_protocol.hrl").
-include("session.hrl").
-include("limits.hrl").

%% Exported functions

-export([
    get_settings/1,
    update_settings/2,
    regenerate_ci_api_key/2
]).

%% API

-spec get_settings(Req) -> Response when
    Req :: cowboy_req:req(),
    Response :: {web_protocol:settings(), cowboy_req:req()}.

get_settings(#{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, Settings} = db_if_settings:get(),
    {web_protocol:settings_from_json(Settings), Req};

get_settings(_Req) ->
    % Unauthorized
    web_rest_admin_settings:get_settings_403(#forbidden_error{}).

-spec update_settings(UpdateRequest, Req) -> Response when
    UpdateRequest :: web_protocol:settings_update_request(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:generic_response(), cowboy_req:req()}.

update_settings(UpdateRequest, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    Patch = web_protocol:settings_update_request_to_json(UpdateRequest),
    Result = case db_if_settings:set(Patch) of
        ok -> true;
        {error, _Reason} -> false
    end,
    {#generic_response{result = Result}, Req};

update_settings(_UpdateRequest, _Req) ->
    % Unauthorized
    web_rest_admin_settings:update_settings_403(#forbidden_error{}).

-spec regenerate_ci_api_key(Empty, Req) -> Response when
    Empty :: web_protocol:empty(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:regenerate_ci_api_key_response(), cowboy_req:req()}.

regenerate_ci_api_key(#empty{}, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    CiApiKey = <<"HLS-CI-", (web_util:uuid64())/binary>>,
    ok = db_if_settings:set_one(ci_api_key, CiApiKey),
    {#regenerate_ci_api_key_response{ci_api_key = CiApiKey}, Req};

regenerate_ci_api_key(_Empty, _Req) ->
    % Unauthorized
    web_rest_admin_settings_ci_api_key:regenerate_ci_api_key_403(#forbidden_error{}).

%% Local functions
