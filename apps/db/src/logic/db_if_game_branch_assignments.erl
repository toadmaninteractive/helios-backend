-module(db_if_game_branch_assignments).

%% Include files

-include("protocol.hrl").

%% Exported functions

-export([
    get_for_branch/1,
    create/2,
    delete/2,
    exists/2
]).

%% API

-spec get_for_branch(BranchId :: non_neg_integer()) ->
    {'ok', [BuildId :: non_neg_integer()]} | {'error', Reason :: atom()}.

get_for_branch(BranchId) ->
    Query = <<"SELECT build_id FROM game_branch_assignments WHERE branch_id = $1">>,
    case db_query:select(Query, [BranchId]) of
        {ok, Items} -> {ok, [BuildId || #{<<"build_id">> := BuildId} <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec create(BranchId :: non_neg_integer(), BuildId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

create(BranchId, BuildId) ->
    Query = <<
        "INSERT INTO game_branch_assignments (branch_id, build_id) ",
        "VALUES ($1, $2) ",
        "ON CONFLICT (branch_id, build_id) DO UPDATE SET assigned_at = current_timestamp"
    >>,
    Params = [BranchId, BuildId],
    case db_query:insert(Query, Params) of
        {ok, _} -> ok;
        {error, Other} -> {error, Other}
    end.

-spec delete(BranchId :: non_neg_integer(), BuildId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

delete(BranchId, BuildId) ->
    Query = <<"DELETE FROM game_branch_assignments WHERE branch_id = $1 AND build_id = $2">>,
    Params = [BranchId, BuildId],
    case db_query:delete(Query, Params) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec exists(BranchId :: non_neg_integer(), BuildId :: non_neg_integer()) ->
    {'ok', boolean()} | {'error', Reason :: atom()}.

exists(BranchId, BuildId) ->
    Statement = <<
        "SELECT count(*)::bigint > 0 AS result ",
        "FROM game_branch_assignments ",
        "WHERE branch_id = $1 AND build_id = $2"
    >>,
    Params = [BranchId, BuildId],
    case db_query:select_one(Statement, Params) of
        {ok, #{<<"result">> := Result}} -> {ok, Result};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions
