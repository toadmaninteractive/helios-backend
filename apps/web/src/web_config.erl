-module(web_config).

%% Include files

-include("settings.hrl").

%% Exported functions

-export([
    bind_ip/0,
    bind_port/0,
    acceptors/0,
    max_upload_size/0,
    proxy_enabled/0,
    secure/0,
    cacertfile/0,
    certfile/0,
    keyfile/0,
    www_root/0,
    url/0,
    cdn_root_url/0,
    cdn_root_url_ssl/0,
    captcha_dir/0,
    uploads_dir/0,
    recaptcha_site_key/0,
    recaptcha_secret_key/0
]).

%% API

-spec bind_ip() ->
    {'ok', inet:ip4_address()} | 'undefined'.

bind_ip() ->
    case application:get_env(?web_app, bind_ip, ?web_ip) of
        {_A, _B, _C, _D} = IpAddressV4 -> {ok, IpAddressV4};
        _ -> undefined
    end.

-spec bind_port() ->
    {'ok', inet:port_number()} | 'undefined'.

bind_port() ->
    case application:get_env(?web_app, bind_port, ?web_port) of
        BindPort when is_integer(BindPort) -> {ok, BindPort};
        _ -> undefined
    end.

-spec acceptors() ->
    {'ok', non_neg_integer()} | 'undefined'.

acceptors() ->
    case application:get_env(?web_app, acceptors, ?web_acceptors) of
        Acceptors when is_integer(Acceptors) -> {ok, Acceptors};
        _ -> undefined
    end.

-spec max_upload_size() ->
    {'ok', non_neg_integer()} | 'undefined'.

max_upload_size() ->
    case application:get_env(?web_app, max_upload_size, ?web_max_upload_size) of
        MaxUploadSize when is_integer(MaxUploadSize) -> {ok, MaxUploadSize};
        _ -> undefined
    end.

-spec proxy_enabled() ->
    {'ok', boolean()} | 'undefined'.

proxy_enabled() ->
    case application:get_env(?web_app, proxy_enabled, false) of
        ProxyEnabled when is_boolean(ProxyEnabled) -> {ok, ProxyEnabled};
        _ -> undefined
    end.

-spec secure() ->
    {'ok', boolean()} | 'undefined'.

secure() ->
    case application:get_env(?web_app, secure, false) of
        Secure when is_boolean(Secure) -> {ok, Secure};
        _ -> undefined
    end.

-spec cacertfile() ->
    {'ok', binary()} | 'undefined'.

cacertfile() ->
    case application:get_env(?web_app, cacertfile, undefined) of
        Binary when is_binary(Binary) -> {ok, Binary};
        List when is_list(List) -> {ok, util_binary:to_binary(List)};
        _ -> undefined
    end.

-spec certfile() ->
    {'ok', binary()} | 'undefined'.

certfile() ->
    case application:get_env(?web_app, certfile, undefined) of
        Binary when is_binary(Binary) -> {ok, Binary};
        List when is_list(List) -> {ok, util_binary:to_binary(List)};
        _ -> undefined
    end.

-spec keyfile() ->
    {'ok', binary()} | 'undefined'.

keyfile() ->
    case application:get_env(?web_app, keyfile, undefined) of
        Binary when is_binary(Binary) -> {ok, Binary};
        List when is_list(List) -> {ok, util_binary:to_binary(List)};
        _ -> undefined
    end.

-spec www_root() ->
    {'ok', binary()} | 'undefined'.

www_root() ->
    case application:get_env(?web_app, www_root, undefined) of
        Binary when is_binary(Binary) -> {ok, Binary};
        List when is_list(List) -> {ok, util_binary:to_binary(List)};
        _ -> undefined
    end.

-spec url() ->
    {'ok', binary()} | 'undefined'.

url() ->
    case application:get_env(?web_app, url, undefined) of
        Binary when is_binary(Binary) -> {ok, Binary};
        List when is_list(List) -> {ok, util_binary:to_binary(List)};
        _ -> undefined
    end.

-spec cdn_root_url() ->
    {'ok', binary()} | 'undefined'.

cdn_root_url() ->
    case application:get_env(?web_app, cdn_root_url, undefined) of
        Binary when is_binary(Binary) -> {ok, Binary};
        List when is_list(List) -> {ok, util_binary:to_binary(List)};
        _ -> undefined
    end.

-spec cdn_root_url_ssl() ->
    {'ok', binary()} | 'undefined'.

cdn_root_url_ssl() ->
    case application:get_env(?web_app, cdn_root_url_ssl, undefined) of
        Binary when is_binary(Binary) -> {ok, Binary};
        List when is_list(List) -> {ok, util_binary:to_binary(List)};
        _ -> undefined
    end.

-spec captcha_dir() ->
    {'ok', binary()} | 'undefined'.

captcha_dir() ->
    case application:get_env(?web_app, captcha_dir, undefined) of
        Str when is_list(Str) -> {ok, iolist_to_binary(Str)};
        BinStr when is_binary(BinStr) -> {ok, BinStr};
        _ -> undefined
    end.

-spec uploads_dir() ->
    {'ok', binary()} | 'undefined'.

uploads_dir() ->
    case application:get_env(?web_app, uploads_dir, undefined) of
        Str when is_list(Str) -> {ok, iolist_to_binary(Str)};
        BinStr when is_binary(BinStr) -> {ok, BinStr};
        _ -> undefined
    end.

-spec recaptcha_site_key() ->
    {'ok', binary()} | 'undefined'.

recaptcha_site_key() ->
    case opt(recaptcha, site_key) of
        {ok, SiteKey} -> {ok, util_binary:to_binary(SiteKey)};
        _ -> undefined
    end.

-spec recaptcha_secret_key() ->
    {'ok', binary()} | 'undefined'.

recaptcha_secret_key() ->
    case opt(recaptcha, secret_key) of
        {ok, SiteKey} -> {ok, util_binary:to_binary(SiteKey)};
        _ -> undefined
    end.

%% Local functions

opt(Group, Opt) ->
    case application:get_env(?web_app, Group, []) of
        PropList when is_list(PropList) ->
            case proplists:get_value(Opt, PropList, undefined) of
                undefined -> undefined;
                Value -> {ok, Value}
            end;
        _ -> undefined
    end.
