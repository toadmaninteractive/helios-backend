-module(web_mw_auth).

-behaviour(cowboy_middleware).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("session.hrl").

%% Exported functions

-export([
    execute/2
]).

%% API

execute(Req, Env) ->
    AuthPrefixes = maps:get(auth_prefixes, Env, []),
    Path = cowboy_req:path(Req),
    Session = case match_prefix(Path, AuthPrefixes) of
        {ok, _Prefix} ->
            % Try to get session IDs either from cookies or HTTP header
            SidFromHeader = cowboy_req:header(?x_session_id, Req, undefined),
            SidFromCookie = proplists:get_value(?x_cookie_sid, cowboy_req:parse_cookies(Req), undefined),
            EncodedSession = case {SidFromHeader, SidFromCookie} of
                {HValue, _} when is_binary(HValue) -> HValue;
                {_, CValue} when is_binary(CValue) -> CValue;
                _ -> undefined
            end,

            % Try to get user ID from API key
            ApiKey = cowboy_req:header(?x_api_key, Req, undefined),

            if
                is_binary(ApiKey) ->
                    case db_if_personnel:get_one_by_api_key(ApiKey) of
                        {ok, #{<<"id">> := UserId}} -> #session{key = {?actor_personnel, undefined}, user_id = UserId};
                        _ -> undefined
                    end;

                is_binary(EncodedSession) ->
                    case web_session:decode(EncodedSession) of
                        % Looks like a valid encoded session
                        {ok, S} -> S;

                        % Does not Look like a valid encoded session OR no session supplied at all
                        {error, invalid} -> undefined;

                        % Looks like an expired encoded session
                        {error, not_exists} -> undefined;

                        % No JWK loaded, crash
                        {error, jwk_not_loaded} -> erlang:error(jwk_not_loaded)
                    end;
                true -> undefined
            end;
        _ -> undefined
    end,
    {ok, Req#{?m_session => Session}, Env}.

%% Local functions

match_prefix(Path, Prefixes) ->
    lists:foldl(fun
        (Prefix, nomatch) ->
            case binary:match(Path, Prefix) of
                {0, _} -> {ok, Prefix};
                _ -> nomatch
            end;
        (_, Acc) ->
            Acc
    end, nomatch, Prefixes).
