-module(server_hub).

-behaviour(gen_server).

%% Include files

-include_lib("aplib/include/apmacros.hrl").

%% Exported functions

-export([
    start_link/0
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

-record(state, {
    worker_pid :: pid() | 'undefined'
}).

-define(cleanup_init_interval, 10 * 1000). % 10 sec
-define(cleanup_retry_interval, 10 * 1000). % 10 sec
-define(cleanup_interval, 60 * 60 * 1000). % 1 hour
-define(cleanup_expired_game_builds, cleanup_expired_game_builds).

%% API

-spec start_link() ->
    {'ok', pid()} | 'ignore' | {'error', {'already_started', pid()} | term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks

init(_) ->
    erlang:send_after(?cleanup_init_interval, self(), ?cleanup_expired_game_builds),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    logger:debug("unhandled call ~p from ~p~n", [Request, From], #{caption => ?MODULE}),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    logger:debug("unhandled cast ~p~n", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

handle_info(?cleanup_expired_game_builds, State) ->
    Pid = proc_lib:spawn(fun cleanup_expired_game_builds/0),
    erlang:monitor(process, Pid),
    {noreply, State#state{worker_pid = Pid}};

handle_info({'DOWN', _MonitorRef, process, Pid, Reason}, #state{worker_pid = Pid} = State) ->
    Interval = ?yesno(Reason =:= normal, ?cleanup_interval, ?cleanup_retry_interval),
    erlang:send_after(Interval, self(), ?cleanup_expired_game_builds),
    {noreply, State#state{worker_pid = undefined}};

handle_info(Msg, State) ->
    logger:debug("unhandled info ~p~n", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local functions

cleanup_expired_game_builds() ->
    {ok, WwwRoot} = web_config:www_root(),
    {ok, Builds} = db_if_game_builds:get_expired(),
    [begin
        BuildDir = filename:join([WwwRoot, GameId, BuildRev]),
        ?doif(filelib:is_dir(BuildDir), util_file:rm_rf(BuildDir)),
        db_if_game_manifests:delete(BuildId),
        Result = db_if_game_builds:wipe(BuildId),
        ?doif(Result =:= ok, logger:info("Deleted expired build <~B> revision #~s for game ~s <~s>", [BuildId, BuildRev, GameTitle, GameId]))
    end || #{<<"id">> := BuildId, <<"build_rev">> := BuildRev, <<"game_id">> := GameId, <<"game_title">> := GameTitle} <- Builds].
