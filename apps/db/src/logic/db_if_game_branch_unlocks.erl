-module(db_if_game_branch_unlocks).

%% Include files

-include("protocol.hrl").

%% Exported functions

-export([
    get_for_client/1,
    get_for_branch/1,
    create/2,
    delete/2,
    exists/2
]).

%% API

-spec get_for_client(ClientId :: non_neg_integer()) ->
    {'ok', [BranchId :: non_neg_integer()]} | {'error', Reason :: atom()}.

get_for_client(ClientId) ->
    Query = <<"SELECT branch_id FROM game_branch_unlocks WHERE client_id = $1">>,
    case db_query:select(Query, [ClientId]) of
        {ok, Items} -> {ok, [BranchId || #{<<"branch_id">> := BranchId} <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec get_for_branch(BranchId :: non_neg_integer()) ->
    {'ok', [ClientId :: non_neg_integer()]} | {'error', Reason :: atom()}.

get_for_branch(BranchId) ->
    Query = <<"SELECT client_id FROM game_branch_unlocks WHERE branch_id = $1">>,
    case db_query:select(Query, [BranchId]) of
        {ok, Items} -> {ok, [ClientId || #{<<"client_id">> := ClientId} <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec create(ClientId :: non_neg_integer(), BranchId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

create(ClientId, BranchId) ->
    Query = <<"INSERT INTO game_branch_unlocks (client_id, branch_id) VALUES ($1, $2)">>,
    Params = [ClientId, BranchId],
    case db_query:insert(Query, Params) of
        {ok, _} -> ok;
        {error, unique_violation} -> {error, ?err_already_exists};
        {error, Other} -> {error, Other}
    end.

-spec delete(ClientId :: non_neg_integer(), BranchId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

delete(ClientId, BranchId) ->
    Query = <<"DELETE FROM game_branch_unlocks WHERE client_id = $1 AND branch_id = $2">>,
    case db_query:delete(Query, [ClientId, BranchId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec exists(ClientId :: non_neg_integer(), BranchId :: non_neg_integer()) ->
    {'ok', boolean()} | {'error', Reason :: atom()}.

exists(ClientId, BranchId) ->
    Statement = <<"SELECT count(*)::bigint AS count FROM game_branch_unlocks WHERE client_id = $1 AND branch_id = $2">>,
    case db_query:select_one(Statement, [ClientId, BranchId]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count > 0};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions
