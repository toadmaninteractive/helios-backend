-module(web_ws_personnel).

-behaviour(cowboy_websocket).

%% Include files

-include_lib("db/include/protocol.hrl").
-include("session.hrl").
-include("notification_protocol.hrl").

%% Exported functions

-export([
    broadcast/1,
    whisper/2,
    disconnect_one/1,
    disconnect_all/0,
    subscribe/1
]).

%% cowboy_websocket callbacks

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2
]).

-define(ws_idle_timeout, 30000).    % 30 sec
-define(ws_ping_interval, 5000).    % 5 sec

-record(state, {
    session :: session()
}).

%% API

-spec broadcast(Notification :: protocol_notification:notification()) -> 'ok'.

broadcast(Notification) ->
    [send_notification(Pid, Notification) || {Pid, _} <- gproc:lookup_local_properties(?actor_personnel)].

-spec whisper(UserId :: non_neg_integer(), Notification :: protocol_notification:notification()) -> 'ok'.

whisper(UserId, Notification) ->
    [send_notification(Pid, Notification) || {Pid, _} <- gproc:lookup_local_properties({personnel_id, UserId})].

-spec disconnect_one(UserId :: non_neg_integer()) -> 'ok'.

disconnect_one(UserId) ->
    [disconnect(Pid) || {Pid, _} <- gproc:lookup_local_properties({personnel_id, UserId})].

-spec disconnect_all() -> 'ok'.

disconnect_all() ->
    [disconnect(Pid) || {Pid, _} <- gproc:lookup_local_properties(?actor_personnel)].

-spec subscribe(NotificationKind :: protocol_notification:notification_kind()) -> true.

subscribe(NotificationKind) ->
    gproc:add_local_property({?actor_personnel, NotificationKind}, undefined).

%% cowboy_websocket callbacks

init(#{?m_session := Session} = Req, _Opts) ->
    {cowboy_websocket, Req, #state{session = Session}, #{idle_timeout => ?ws_idle_timeout}}.

websocket_init(#state{session = #session{key = {UserType, _SessionId}, user_id = UserId}} = State) when UserType =:= ?actor_personnel ->
    % Register websocket
    gproc:add_local_property(?actor_personnel, UserType),
    gproc:add_local_property({personnel_id, UserId}, undefined),

    % Send hello notification and launch heartbeat cycle
    erlang:send_after(0, self(), {send, text, pack(#hello{})}),
    erlang:send_after(?ws_ping_interval, self(), heartbeat),
    % logger:debug("Websocket connected: user ID = ~p, pid = ~p", [UserId, self()], #{caption => ?MODULE}),
    {ok, State};

%% Unauthorized

websocket_init(State) ->
    {stop, State}.

websocket_handle({text, Msg}, #state{session = #session{key = {UserType, SessionId}, user_id = UserId}} = State) ->
    Json = web_util:decode_json(Msg),
    Notification = notification_protocol:notification_from_json(Json),
    NotificationKind = element(1, Notification),
    publish(NotificationKind, Notification, UserType, UserId, SessionId),
    {ok, State};
websocket_handle({ping, _Msg}, State) ->
    {ok, State};
websocket_handle({pong, _Msg}, State) ->
    {ok, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info(heartbeat, State) ->
    erlang:send_after(?ws_ping_interval, self(), heartbeat),
    {reply, ping, State};
websocket_info(ping, State) ->
    {reply, ping, State};
websocket_info({send, text, Msg}, State) ->
    {reply, {text, Msg}, State};
websocket_info({send, binary, Msg}, State) ->
    {reply, {binary, Msg}, State};
websocket_info(disconnect, State) ->
    {stop, State};
websocket_info(_Info, State) ->
    {ok, State}.

%% Local functions
pack(Notification) ->
    Json = notification_protocol:notification_to_json(Notification),
    jsx:encode(Json).

send(HandlerPid, Type, Message) ->
    HandlerPid ! {send, Type, Message}.

send_text(Pid, Message) ->
    send(Pid, text, Message).

%% send_binary(Pid, Message) ->
%%     send(Pid, binary, Message).

send_notification(Pid, Notification) ->
    send_text(Pid, pack(Notification)).

%% ping(Pid) ->
%%     Pid ! ping.

disconnect(Pid) ->
    Pid ! disconnect.

publish(NotificationKind, Notification, UserType, UserId, SessionId) ->
    % Subscribed processes will get the following message: {{UserType, NotificationKind}, {Notification, UserId, SessionId, WebsocketPid}}
    % Example usage: web_ws_personnel:subscribe(hello)
    Key = {UserType, NotificationKind},
    gproc:send({p, l, Key}, {Key, {Notification, UserId, SessionId, self()}}).
