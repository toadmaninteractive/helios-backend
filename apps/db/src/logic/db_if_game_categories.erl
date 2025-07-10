-module(db_if_game_categories).

%% Include files

-include_lib("epgsql/include/epgsql.hrl").
-include_lib("aplib/include/apmacros.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get_all/0,
    create/3,
    update/2,
    update/3,
    delete/1,
    exists/1
]).

%% API

-spec get_one(CategoryId :: non_neg_integer()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one(CategoryId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_categories AS gc ",
        (common_joins())/binary,
        "WHERE gc.id = $1"
    >>,
    case db_query:select_one(Query, [CategoryId]) of
        {ok, Item} -> {ok, Item};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_all() ->
    {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_all() ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_categories AS gc ",
        (common_joins())/binary,
        "ORDER BY id ASC"
    >>,
    db_query:select(Query, []).

-spec create(Name :: binary(), Description :: binary(), SortOrder :: non_neg_integer()) ->
    {'ok', CategoryId :: binary()} | {'error', Reason :: atom()}.

create(Name, Description, SortOrder) ->
    Query = <<
        "INSERT INTO game_categories (name, description, sort_order) ",
        "VALUES (TRIM($1), TRIM($2), $3) ",
        "RETURNING id::bigint"
    >>,
    Params = [Name, Description, SortOrder],
    case db_query:insert(Query, Params, [raw_error]) of
        {ok, 1, Columns, Rows} ->
            [#{<<"id">> := GameId}] = db_util:result_to_json(Columns, Rows),
            {ok, GameId};
        {error, #error{codename = unique_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"gc_name_ult_index">> -> {error, ?err_already_exists}
            end;
        {error, #error{codename = check_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"game_categories_name_check">> -> {error, invalid_name}
            end;
        {error, #error{codename = Code}} ->
            {error, Code}
    end.

-spec update(CategoryId :: non_neg_integer(), Patch :: maps:map()) ->
    'ok' | {'error', Reason :: atom()}.

update(CategoryId, Patch) ->
    update(CategoryId, undefined, Patch).

-spec update(CategoryId :: non_neg_integer(), Rev :: non_neg_integer() | 'undefined', Patch :: maps:map()) ->
    'ok' | {'error', Reason :: atom()}.

update(CategoryId, Rev, Patch) ->
    % Define all possible fields
    Fields = [
        ?mk_mod_trim(name),
        ?mk_mod_trim(description),
        sort_order
    ],

    % Define auto-set fields
    AutoSetFields = [
        ?mk_mod_inc(rev),
        ?mk_mod_set_now(updated_at)
    ],

    % Perform update
    case db_util:mk_update(<<"game_categories">>, <<"id">>, CategoryId, <<"rev">>, Rev, Fields, AutoSetFields, Patch) of
        {ok, Query, Params} ->
            case db_query:update(Query, Params, [raw_error]) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, not_exists};
                {error, #error{codename = unique_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"gc_name_ult_index">> -> {error, ?err_already_exists}
                    end;
                {error, #error{codename = check_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"game_categories_name_check">> -> {error, invalid_name}
                    end;
                {error, #error{codename = Code}} -> {error, Code}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete(CategoryId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

delete(CategoryId) ->
    Query = <<"DELETE FROM game_categories WHERE id = $1">>,
    case db_query:delete(Query, [CategoryId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec exists(CategoryId :: non_neg_integer()) ->
    {'ok', boolean()} | {'error', Reason :: atom()}.

exists(CategoryId) ->
    Statement = <<"SELECT count(*)::bigint > 0 AS result FROM game_categories WHERE id = $1">>,
    case db_query:select_one(Statement, [CategoryId]) of
        {ok, #{<<"result">> := Result}} -> {ok, Result};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % gc : game_categories
    <<
        "gc.id AS id, ",
        "gc.rev AS rev, ",
        "gc.name AS name, ",
        "gc.description AS description, ",
        "gc.sort_order AS sort_order, ",
        "gc.created_at AS created_at, ",
        "gc.updated_at AS updated_at "
    >>.

common_joins() ->
    <<>>.
