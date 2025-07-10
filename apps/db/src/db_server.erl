-module(db_server).

-behaviour(gen_server).

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

-record(state, {}).

%% API

-spec start_link() ->
    {'ok', pid()} | 'ignore' | {'error', {'already_started', pid()} | term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks

init(_) ->
    {ok, #state{}}.

handle_call(Request, From, State) ->
    logger:debug("unhandled call ~p from ~p~n", [Request, From], #{caption => ?MODULE}),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    logger:debug("unhandled cast ~p~n", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

handle_info(Msg, State) ->
    logger:debug("unhandled info ~p~n", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local functions
