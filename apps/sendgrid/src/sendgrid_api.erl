-module(sendgrid_api).

%% Exported functions

-export([
    sendmail/4
]).

%% API

-spec sendmail(ToEmail, ToName, TemplateId, DynamicTemplateData) -> Result when
    ToEmail :: binary(),
    ToName :: binary(),
    TemplateId :: binary(),
    DynamicTemplateData :: jsx:json_term(),
    Result :: any().

sendmail(ToEmail, ToName, TemplateId, DynamicTemplateData) ->
    ApiHost = binary_to_list(sendgrid_config:api_host()),
    ApiKey = binary_to_list(sendgrid_config:api_key()),
    FromEmail = sendgrid_config:from_email(),
    FromName = sendgrid_config:from_name(),
    ReqHeaders = [
        {"Authorization", "Bearer " ++ ApiKey},
        {"Content-Type", "application/json"},
        {"Host", ApiHost}
    ],
    Json = #{
        personalizations => [
            #{
                to => [#{email => ToEmail, name => ToName}],
                dynamic_template_data => DynamicTemplateData
            }
        ],
        from => #{email => FromEmail, name => FromName},
        reply_to => #{email => FromEmail, name => FromName},
        template_id => TemplateId
    },
    Url = make_url("https", ApiHost, "/v3/mail/send", []),
    Options = [
        {sync, true},
        {headers_as_is, true},
        {connect_timeout, 3000},
        {response_format, binary}
    ],
    case ibrowse:send_req(Url, ReqHeaders, post, jsx:encode(Json), Options, 5000) of
        {ok, Status, RespHeaders, _Body} when Status =:= "200"; Status =:= "202"->
            MessageId = proplists:get_value("X-Message-Id", RespHeaders, <<>>),
            {ok, MessageId};
        {ok, "401", _Headers, _Body} ->
            {error, invalid_api_key};
        {error, {conn_failed, error}} ->
            {error, connection_failed};
        {error, {conn_failed, {error,timeout}}} ->
            {error, connection_timeout};
        {error, req_timedout} ->
            {error, request_timeout}
    end.

%% Local functions

make_url(Protocol, Host, Path, Params) ->
    Path1 = case Path of [$/|Rest] -> Rest; _ -> Path end,
    lists:flatten([Protocol, "://", Host, "/", Path1, ["?" || Params =/= []], make_params(Params)]).

make_params(Params) ->
    join([[ibrowse_lib:url_encode(K), "=", ibrowse_lib:url_encode(util_lists:to_list(V))] || {K, V} <- Params], "&").

join([], _Separator) ->
    [];
join([H|Tail], Separator) ->
    [H, [[Separator, I] || I <- Tail]].
