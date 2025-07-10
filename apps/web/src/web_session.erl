-module(web_session).

-behaviour(gen_server).

%% Include files

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("db/include/protocol.hrl").
-include("settings.hrl").
-include("session.hrl").
-include("kv.hrl").

%% Exported functions

-export([
    start_link/0,
    get/2,
    create/2,
    prolong/2,
    delete/2,
    delete_for/2,
    encode/2,
    decode/1
]).

%% gen_server callbacks

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(refresh_interval, 10*1000). % 10 sec
-define(ets_sessions, web_session).

-record(state, {}).

%% API

-spec start_link() ->
    {'ok', pid()} | 'ignore' | {'error', {'already_started', pid()} | term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get(UserType :: 'personnel' | 'client', SessionId :: non_neg_integer()) ->
    {'ok', session()} | {'error', 'not_exists'}.

get(UserType, SessionId) when UserType =:= ?actor_personnel; UserType =:= ?actor_client ->
    case ets:lookup(?ets_sessions, {UserType, SessionId}) of
        [#session{} = Session|_] -> {ok, Session};
        _ -> {error, not_exists}
    end.

-spec create(UserType :: 'personnel' | 'client', UserId :: non_neg_integer()) ->
    {'ok', SessionId :: binary()}.

create(UserType, UserId) when UserType =:= ?actor_personnel; UserType =:= ?actor_client ->
    % Generate unique session ID
    SessionId = web_util:uuid64(),

    % Get session litetime
    {ok, Lifetime} = case UserType of
        ?actor_personnel -> db_if_settings:personnel_session_duration();
        ?actor_client -> db_if_settings:client_session_duration()
    end,

    % Store session in database
    ok = case UserType of
        ?actor_personnel -> db_if_personnel_sessions:create(SessionId, UserId, Lifetime);
        ?actor_client -> db_if_client_sessions:create(SessionId, UserId, Lifetime)
    end,

    % Store session in ETS
    Now = util_time:utc_seconds(),
    ok = resume(UserType, SessionId, UserId, Now, Now + Lifetime),
    {ok, SessionId}.

-spec prolong(UserType :: 'personnel' | 'client', SessionId :: binary()) ->
    'ok' | {'error', 'not_exists'}.

prolong(UserType, SessionId) when UserType =:= ?actor_personnel; UserType =:= ?actor_client ->
    case get(UserType, SessionId) of
        {ok, Session} ->
            % Get session litetime
            {ok, Lifetime} = case UserType of
                ?actor_personnel -> db_if_settings:personnel_session_duration();
                ?actor_client -> db_if_settings:client_session_duration()
            end,

            % Update session in ETS and database
            Now = util_time:utc_seconds(),
            ets:insert(?ets_sessions, Session#session{valid_thru = Now + Lifetime}),
            case UserType of
                ?actor_personnel -> db_if_personnel_sessions:prolong(SessionId, Lifetime);
                ?actor_client -> db_if_client_sessions:prolong(SessionId, Lifetime)
            end;
        {error, _Reason} = Error ->
            Error
    end.

-spec delete(UserType :: 'personnel' | 'client', SessionId :: binary()) ->
    'ok' | {'error', 'not_exists'}.

delete(UserType, SessionId) when UserType =:= ?actor_personnel; UserType =:= ?actor_client ->
    case get(UserType, SessionId) of
        {ok, #session{key = Key}} ->
            % Delete from ETS and database
            ets:delete(?ets_sessions, Key),
            case UserType of
                ?actor_personnel -> db_if_personnel_sessions:delete(SessionId);
                ?actor_client -> db_if_client_sessions:delete(SessionId)
            end;
        {error, _Reason} = Error ->
            Error
    end.

-spec delete_for(UserType :: 'personnel' | 'client', UserId :: non_neg_integer() | ?null) ->
    'ok'.

delete_for(UserType, UserId) when UserType =:= ?actor_personnel; UserType =:= ?actor_client ->
    % Delete all sessions in ETS
    MatchSpecDelete = ets:fun2ms(fun(#session{key = SessionKey}) when SessionKey =:= {UserType, UserId} -> true end),
    ets:select_delete(?ets_sessions, MatchSpecDelete),
    case UserType of
        ?actor_personnel -> db_if_personnel_sessions:delete_for(UserId);
        ?actor_client -> db_if_client_sessions:delete_for(UserId)
    end,
    ok.

-spec encode(UserType :: 'personnel' | 'client', SessionId :: binary()) ->
    {'ok', EncodedSession :: binary()} | {'error', 'jwk_not_loaded'}.

encode(UserType, SessionId) when UserType =:= ?actor_personnel; UserType =:= ?actor_client ->
    try
        JWK = web_storage:jwk(),
        JsonStr = web_util:encode_json(#{user_type => UserType, session_id => SessionId}),
        EncodedSession = jws:encode_compact(JsonStr, #{alg => <<"RS256">>}, JWK),
        {ok, EncodedSession}
    catch _Type:_What:_StackTrace ->
        {error, jwk_not_loaded}
    end.

-spec decode(EncodedSession :: binary()) ->
    {'ok', Session :: session()} | {'error', 'jwk_not_loaded' | 'not_exists' | 'invalid'}.

decode(EncodedSession) ->
    try
        % Decode session in a safe manner
        JWK = web_storage:jwk(),
        Result = try
            {true, JsonStr, _} = jws:decode_compact(EncodedSession, JWK),
            web_util:decode_json_safe(JsonStr, undefined)
        catch
            _C:_R:_S -> undefined
        end,
        DecodedResult = case Result of
            #{<<"user_type">> := <<"personnel">>, <<"session_id">> := Sid} -> {?actor_personnel, Sid};
            #{<<"user_type">> := <<"client">>, <<"session_id">> := Sid} -> {?actor_client, Sid};
            _ -> {error, invalid}
        end,
        case DecodedResult of
            {error, invalid} ->
                {error, invalid};
            {UserType, SessionId} ->
                case get(UserType, SessionId) of
                    {ok, #session{} = Session} -> {ok, Session};
                    {error, not_exists} -> {error, not_exists}
                end
        end
    catch _Type:_What:_StackTrace ->
        {error, jwk_not_loaded}
    end.

%% gen_server callbacks

init(_Args) ->
    % Create ETS tables, cleanup expires and preload valid sessions
    ets:new(?ets_sessions, [set, public, named_table, {keypos, #session.key}]),
    cleanup_sessions(),
    preload_sessions(),
    erlang:send_after(?refresh_interval, self(), refresh),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    logger:debug("unhandled call ~p from ~p~n", [Request, From], #{caption => ?MODULE}),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    logger:debug("unhandled cast ~p~n", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

handle_info(refresh, State) ->
    % Clean up expired sessions in ETS
    Now = util_time:utc_seconds(),
    MatchSpecDelete = ets:fun2ms(fun(#session{valid_thru = ValidThru}) when ValidThru < Now -> true end),
    ets:select_delete(?ets_sessions, MatchSpecDelete),

    % Clean up expired sessions in database
    cleanup_sessions(),

    % Repeat refresh after certain interval
    erlang:send_after(?refresh_interval, self(), refresh),
    {noreply, State};

handle_info(Msg, State) ->
    logger:debug("unhandled info ~p~n", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local functions

cleanup_sessions() ->
    % Clean up expired sessions in database
    db_if_personnel_sessions:cleanup_expired(),
    db_if_client_sessions:cleanup_expired().

preload_sessions() ->
    % Load client sessions from database and resume them
    case db_if_client_sessions:get() of
        {ok, CNS} when is_list(CNS) ->
            [resume(client, SessionId, ClientId, CreatedAt, ValidThru) || #{
                <<"id">> := SessionId,
                <<"client_id">> := ClientId,
                <<"created_at">> := CreatedAt,
                <<"valid_thru">> := ValidThru
            } <- CNS];
        {error, _} -> ignore
    end,

    % Load personnel sessions from database and resume them
    case db_if_personnel_sessions:get() of
        {ok, ANS} when is_list(ANS) ->
            [resume(personnel, SessionId, PersonnelId, CreatedAt, ValidThru) || #{
                <<"id">> := SessionId,
                <<"personnel_id">> := PersonnelId,
                <<"created_at">> := CreatedAt,
                <<"valid_thru">> := ValidThru
            } <- ANS];
        {error, _} -> ignore
    end.

resume(UserType, SessionId, UserId, CreatedAt, ValidThru) when UserType =:= ?actor_personnel; UserType =:= ?actor_client ->
    Session = #session{
        key = {UserType, SessionId},
        user_id = UserId,
        created_at = CreatedAt,
        valid_thru = ValidThru
    },
    ets:insert(?ets_sessions, Session),
    ok.
