-module(db_if_game_manifests).

%% Include files

-include("protocol.hrl").

%% Exported functions

-export([
    get/1,
    add/2,
    delete/1
]).

%% API

-spec get(BuildId :: non_neg_integer()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get(BuildId) ->
    Query = <<"SELECT manifest FROM game_manifests WHERE build_id = $1">>,
    case db_query:select_one(Query, [BuildId]) of
        {ok, #{<<"manifest">> := Manifest}} -> {ok, Manifest};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec add(BuildId :: non_neg_integer(), Manifest :: jsx:json_term()) ->
    'ok' | {'error', Reason :: atom()}.

add(BuildId, Manifest) ->
    Query = <<
        "INSERT INTO game_manifests (build_id, manifest) ",
        "VALUES ($1, $2) ",
        "ON CONFLICT (\"build_id\") DO UPDATE SET manifest = EXCLUDED.manifest, updated_at = current_timestamp "
    >>,
    Params = [BuildId, jsx:encode(Manifest)],
    case db_query:insert(Query, Params) of
        {ok, _} -> ok;
        {error, unique_violation} -> {error, ?err_already_exists};
        {error, Other} -> {error, Other}
    end.

-spec delete(BuildId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

delete(BuildId) ->
    Query = <<"DELETE FROM game_manifests WHERE build_id = $1">>,
    case db_query:delete(Query, [BuildId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

%% Local functions
