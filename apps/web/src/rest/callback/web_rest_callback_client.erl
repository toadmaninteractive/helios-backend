-module(web_rest_callback_client).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("web_protocol.hrl").
-include("session.hrl").

%% Exported functions

-export([
    get_client_status/1,
    register_client/2,
    confirm_client_registration/2,
    resend_registration_code/2,
    login_client/2,
    logout_client/2,
    reset_client_password/2,
    confirm_client_password_reset/2,
    change_client_password/2
]).

%% API

-spec get_client_status(Req) -> Response when
    Req :: cowboy_req:req(),
    Response :: {web_protocol:client_status_response(), cowboy_req:req()}.

get_client_status(#{?m_session := #session{key = {?actor_client, _SessionId}, user_id = UserId}} = Req) ->
    {ok, #{<<"email">> := Email, <<"username">> := Username}} = db_if_clients:get_one(UserId),
    {#client_status_response{logged_in = true, email = Email, username = Username, user_id = UserId}, Req};
get_client_status(Req) ->
    {#client_status_response{logged_in = false}, Req}.

-spec register_client(Request, Req) -> Response when
    Request :: web_protocol:client_register_request(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:client_register_response(), cowboy_req:req()}.

register_client(_Request, #{?m_session := #session{}} = Req) ->
    {#client_login_response{result = false, error = already_logged_in}, Req};
register_client(#client_register_request{email = Email, username = Username, password = Password, captcha_key = CaptchaKey, captcha_answer = CaptchaAnswer}, Req) ->
    IsEmailValid = util_validate:email(Email),
    IsUsernameValid = util_validate:username(Username),
    IsPasswordValid = util_validate:non_empty_string(Password),
    % TODO: use CAPTCHA always or make its usage a settings parameter
    IsCaptchaUsed = is_binary(CaptchaKey),
    CaptchaCheckResult = web_captcha:validate(CaptchaKey, CaptchaAnswer),
    IsCaptchaValid = ?yesno(IsCaptchaUsed, CaptchaCheckResult =:= ok, true),
    Response = if
        not IsEmailValid -> #client_register_response{result = false, error = invalid_email};
        not IsUsernameValid -> #client_register_response{result = false, error = invalid_username};
        not IsPasswordValid -> #client_register_response{result = false, error = invalid_password};
        not IsCaptchaValid, CaptchaCheckResult =:= {error, invalid_captcha_key} -> #client_register_response{result = false, error = invalid_captcha_key};
        not IsCaptchaValid, CaptchaCheckResult =:= {error, invalid_captcha_response} -> #client_register_response{result = false, error = invalid_captcha_response};
        not IsCaptchaValid, CaptchaCheckResult =:= {error, captcha_expired} -> #client_register_response{result = false, error = captcha_expired};
        true ->
            case db_if_clients:create(Username, Password, Email) of
                {ok, UserId} ->
                    SecurityCode = web_util:security_code(),
                    ok = db_if_client_requests:set_register_confirm(UserId, SecurityCode),
                    sendgrid:notify_register(Email, Username, SecurityCode),
                    #client_register_response{result = true};
                {error, ?err_already_exists} ->
                    {ok, IsEmailRegistered} = db_if_clients:email_exists(Email),
                    {ok, IsUsernameRegistered} = db_if_clients:username_exists(Username),
                    if
                        IsEmailRegistered -> #client_register_response{result = false, error = email_already_registered};
                        IsUsernameRegistered -> #client_register_response{result = false, error = username_already_registered};
                        true -> #client_register_response{result = false, error = failure}
                    end
            end
    end,
    ?doif(IsCaptchaUsed, web_captcha:delete(CaptchaKey)),
    {Response, Req}.

-spec confirm_client_registration(Request, Req) -> Response when
    Request :: web_protocol:client_register_confirm_request(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:generic_response(), cowboy_req:req()}.

confirm_client_registration(_Request, #{?m_session := #session{}} = Req) ->
    {#generic_response{result = false}, Req};
confirm_client_registration(#client_register_confirm_request{username = Username, security_code = SecurityCode}, Req) ->
    try
        {ok, #{<<"id">> := UserId, <<"email">> := Email}} = db_if_clients:get_one_by_username(Username),
        {ok, SecurityCode} = db_if_client_requests:register_confirm(UserId),
        ok = db_if_clients:activate(UserId),
        ok = db_if_client_requests:delete_register_confirm(UserId),
        sendgrid:notify_register_confirm(Email, Username),
        {#generic_response{result = true}, Req}
    catch
        _C:_R:_S -> {#generic_response{result = false}, Req}
    end.

-spec resend_registration_code(Request, Req) -> Response when
    Request :: web_protocol:client_resend_request(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:generic_response(), cowboy_req:req()}.

resend_registration_code(_Request, #{?m_session := #session{}} = Req) ->
    {#generic_response{result = false}, Req};
resend_registration_code(#client_resend_request{username = Username}, Req) ->
    try
        {ok, #{<<"id">> := UserId, <<"email">> := Email, <<"is_activated">> := IsActivated}} = db_if_clients:get_one_by_username(Username),
        ?doif(not IsActivated, begin
            SecurityCode = web_util:security_code(),
            ok = db_if_client_requests:set_register_confirm(UserId, SecurityCode),
            sendgrid:notify_register(Email, Username, SecurityCode)
        end),
        {#generic_response{result = not IsActivated}, Req}
    catch
        _C:_R:_S -> {#generic_response{result = false}, Req}
    end.

-spec login_client(Request, Req) -> Response when
    Request :: web_protocol:client_login_request(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:client_login_response(), cowboy_req:req()}.

login_client(_Request, #{?m_session := #session{}} = Req) ->
    {#client_login_response{result = false, error = already_logged_in}, Req};
login_client(#client_login_request{username = ReqUsername, password = ReqPassword}, Req) ->
    try
        case db_if_clients:credentials_by_username(ReqUsername) of
            {ok, Credentials} ->
                #{
                    <<"id">> := UserId,
                    <<"password">> := PasswordHash,
                    <<"salt">> := Salt,
                    <<"is_activated">> := IsActivated,
                    <<"is_blocked">> := IsBlocked,
                    <<"is_deleted">> := IsDeleted
                } = Credentials,
                if
                    not IsActivated ->
                        {#client_login_response{result = false, error = account_not_activated}, Req};
                    IsBlocked ->
                        {#client_login_response{result = false, error = account_is_blocked}, Req};
                    IsDeleted ->
                        {#client_login_response{result = false, error = account_is_deleted}, Req};
                    true ->
                        case db_util:password_hash(ReqPassword, Salt) of
                            PasswordHash ->
                                {ok, SessionId} = web_session:create(?actor_client, UserId),
                                {ok, EncodedSession} = web_session:encode(?actor_client, SessionId),
                                Req1 = cowboy_req:set_resp_header(?x_session_id_cs, EncodedSession, Req),
                                {ok, SessionDuration} = db_if_settings:client_session_duration(),
                                Req2 = cowboy_req:set_resp_cookie(?x_cookie_sid, EncodedSession, Req1, #{path => <<"/">>, max_age => SessionDuration}),
                                {#client_login_response{result = true, session_id = EncodedSession}, Req2};
                            _ ->
                                {#client_login_response{result = false, error = invalid_password}, Req}
                        end

                end;
            {error, ?err_not_exists} ->
                {#client_login_response{result = false, error = account_not_exists}, Req}
        end
    catch
        _C:_R:_S -> {#client_login_response{result = false, error = failure}, Req}
    end.

-spec logout_client(Request, Req) -> Response when
    Request :: web_protocol:empty(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:generic_response(), cowboy_req:req()}.

logout_client(_Request, #{?m_session := #session{key = {?actor_client, SessionId}}} = Req) ->
    web_session:delete(?actor_client, SessionId),
    Req1 = cowboy_req:set_resp_cookie(?x_cookie_sid, <<>>, Req, #{path => <<"/">>, max_age => 0}),
    {#generic_response{result = true}, Req1};
logout_client(_Request, Req) ->
    {#generic_response{result = false}, Req}.

-spec reset_client_password(Request, Req) -> Response when
    Request :: web_protocol:client_password_reset_request(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:client_password_reset_response(), cowboy_req:req()}.

reset_client_password(#client_password_reset_request{username = Username, new_password = NewPassword, captcha_key = CaptchaKey, captcha_answer = CaptchaAnswer}, Req) ->
    IsPasswordValid = util_validate:non_empty_string(NewPassword),
    % TODO: use CAPTCHA always or make its usage a settings parameter
    IsCaptchaUsed = is_binary(CaptchaKey),
    CaptchaCheckResult = web_captcha:validate(CaptchaKey, CaptchaAnswer),
    IsCaptchaValid = ?yesno(IsCaptchaUsed, CaptchaCheckResult =:= ok, true),
    Response = if
        not IsPasswordValid -> #client_password_reset_response{result = false, error = invalid_new_password};
        not IsCaptchaValid, CaptchaCheckResult =:= {error, invalid_captcha_key} -> #client_password_reset_response{result = false, error = invalid_captcha_key};
        not IsCaptchaValid, CaptchaCheckResult =:= {error, invalid_captcha_response} -> #client_password_reset_response{result = false, error = invalid_captcha_response};
        not IsCaptchaValid, CaptchaCheckResult =:= {error, captcha_expired} -> #client_password_reset_response{result = false, error = captcha_expired};
        true ->
            try
                {ok, #{<<"id">> := UserId, <<"email">> := Email}} = db_if_clients:get_one_by_username(Username),
                SecurityCode = web_util:security_code(),
                Salt = db_util:salt(),
                PasswordHash = db_util:password_hash(NewPassword, Salt),
                ok = db_if_client_requests:set_password_reset_confirm(UserId, SecurityCode, PasswordHash, Salt),
                sendgrid:notify_password_reset(Email, Username, SecurityCode),
                #client_password_reset_response{result = true}
            catch _C:_R:_S ->
                #client_password_reset_response{result = false, error = failure}
            end
    end,
    ?doif(IsCaptchaUsed, web_captcha:delete(CaptchaKey)),
    {Response, Req}.

-spec confirm_client_password_reset(Request, Req) -> Response when
    Request :: web_protocol:client_password_reset_confirm_request(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:generic_response(), cowboy_req:req()}.

confirm_client_password_reset(#client_password_reset_confirm_request{username = Username, security_code = SecurityCode}, Req) ->
    try
        {ok, #{<<"id">> := UserId}} = db_if_clients:get_one_by_username(Username),
        {ok, SecurityCode, NewPasswordHash, NewSalt} = db_if_client_requests:password_reset_confirm(UserId),
        ok = db_if_clients:update_password_hash(UserId, NewSalt, NewPasswordHash),
        ok = db_if_client_requests:delete_password_reset_confirm(UserId),
        {#generic_response{result = true}, Req}
    catch
        _C:_R:_S -> {#generic_response{result = false}, Req}
    end.

-spec change_client_password(Request, Req) -> Response when
    Request :: web_protocol:client_password_change_request(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:client_password_change_response(), cowboy_req:req()}.

change_client_password(#client_password_change_request{current_password = CurrentPassword, new_password = NewPassword}, #{?m_session := #session{key = {?actor_client, _}, user_id = UserId}} = Req) ->
    {ok, #{<<"password">> := PasswordHash, <<"salt">> := Salt}} = db_if_clients:credentials(UserId),
    IsValidCurrentPassword = db_util:password_hash(CurrentPassword, Salt) =:= PasswordHash,
    IsValidNewPassword = util_validate:non_empty_string(NewPassword),
    Response = if
        not IsValidCurrentPassword ->
            #client_password_change_response{result = false, error = invalid_current_password};
        not IsValidNewPassword ->
            #client_password_change_response{result = false, error = invalid_new_password};
        true ->
            try
                ok = db_if_clients:update_password(UserId, NewPassword),
                #client_password_change_response{result = true}
            catch _C:_R:_S ->
                #client_password_change_response{result = false, error = failure}
            end
    end,
    {Response, Req};
change_client_password(_Request, Req) ->
    {#client_password_change_response{result = false, error = not_logged_in}, Req}.

%% Local functions
