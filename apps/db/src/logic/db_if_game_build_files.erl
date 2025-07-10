-module(db_if_game_build_files).

%% Include files

-include_lib("epgsql/include/epgsql.hrl").
-include_lib("aplib/include/apmacros.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get_all/1,
    get/5,
    get_count/1,
    create/6,
    delete/1,
    delete/2,
    delete_all/1,
    exists/2
]).

%% API

-spec get_one(FileId :: non_neg_integer()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one(FileId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_build_files AS gblf ",
        (common_joins())/binary,
        "WHERE gblf.id = $1 "
    >>,
    case db_query:select_one(Query, [FileId]) of
        {ok, Item} -> {ok, Item};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_all(BuildId :: non_neg_integer()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_all(BuildId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_build_files AS gblf ",
        (common_joins())/binary,
        "WHERE gblf.build_id = $1 ",
        "ORDER BY file_path ASC"
    >>,
    case db_query:select(Query, [BuildId]) of
        {ok, Item} -> {ok, Item};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get(BuildId, OrderBy, OrderDir, Offset, Limit) -> Result when
    BuildId :: non_neg_integer(),
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get(BuildId, OrderBy, OrderDir, Offset, Limit) ->
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Statement = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_build_files AS gblf ",
        (common_joins())/binary,
        "WHERE gblf.build_id = $1 ",
        Filter/binary
    >>,
    db_query:select(Statement, [BuildId]).

-spec get_count(BuildId :: non_neg_integer()) ->
    {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_count(BuildId) ->
    Statement = <<"SELECT count(*)::bigint AS count FROM game_build_files WHERE build_id = $1">>,
    case db_query:select_one(Statement, [BuildId]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec create(BuildId, FilePath, FileSize, CompressedFilePath, CompressedFileSize, Md5) -> Result when
    BuildId :: non_neg_integer(),
    FilePath :: binary(),
    FileSize :: non_neg_integer(),
    CompressedFilePath :: binary(),
    CompressedFileSize :: non_neg_integer(),
    Md5 :: binary(),
    Result :: {'ok', FileId :: non_neg_integer()} | {'error', Reason :: atom()}.

create(BuildId, FilePath, FileSize, CompressedFilePath, CompressedFileSize, Md5) ->
    Query = <<
        "INSERT INTO game_build_files (build_id, file_path, file_size, compressed_file_path, compressed_file_size, md5) ",
        "VALUES ($1, TRIM($2), $3, TRIM($4), $5, LOWER(TRIM($6))) ",
        "ON CONFLICT (\"build_id\", \"file_path\") DO UPDATE ",
        "SET ",
            "file_size = EXCLUDED.file_size, ",
            "compressed_file_path = EXCLUDED.compressed_file_path, ",
            "compressed_file_size = EXCLUDED.compressed_file_size, ",
            "md5 = EXCLUDED.md5, ",
            "updated_at = current_timestamp ",
        "RETURNING id::bigint"
    >>,
    Params = [BuildId, FilePath, FileSize, CompressedFilePath, CompressedFileSize, Md5],
    case db_query:insert(Query, Params, [raw_error]) of
        {ok, 1, Columns, Rows} ->
            [#{<<"id">> := FileId}] = db_util:result_to_json(Columns, Rows),
            {ok, FileId};
        {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"game_build_files_build_id_fkey">> -> {error, invalid_build_id}
            end;
        {error, #error{codename = check_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"game_build_files_file_size_check">> ->  {error, invalid_file_size};
                <<"game_build_files_compressed_file_size_check">> ->  {error, invalid_compressed_file_size}
            end;
%        {error, #error{codename = unique_violation, extra = ExtraProps}} ->
%            case proplists:get_value(constraint_name, ExtraProps, undefined) of
%                <<"gblf_build_file_uc_index">> -> {error, build_file_already_exists}
%            end;
        {error, #error{codename = Code}} ->
            {error, Code}
    end.

-spec delete(FileId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

delete(FileId) ->
    Query = <<"DELETE FROM game_build_files WHERE id = $1">>,
    case db_query:delete(Query, [FileId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec delete(BuildId :: non_neg_integer(), FilePath :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

delete(BuildId, FilePath) ->
    Query = <<"DELETE FROM game_build_files WHERE build_id = $1 AND file_path = $2">>,
    case db_query:delete(Query, [BuildId, FilePath]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec delete_all(BuildId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

delete_all(BuildId) ->
    Query = <<"DELETE FROM game_build_files WHERE build_id = $1">>,
    case db_query:delete(Query, [BuildId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec exists(BuildId :: non_neg_integer(), FilePath :: binary()) ->
    {'ok', boolean()} | {'error', Reason :: atom()}.

exists(BuildId, FilePath) ->
    Statement = <<"SELECT count(*)::bigint AS count FROM game_build_files WHERE build_id = $1 AND file_path = TRIM($2)">>,
    case db_query:select_one(Statement, [BuildId, FilePath]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count > 0};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % gblf : game_build_files
    <<
        "gblf.id AS id, ",
        "gblf.build_id AS build_id, ",
        "gblf.file_path AS file_path, ",
        "gblf.file_size AS file_size, ",
        "gblf.compressed_file_path AS compressed_file_path, ",
        "gblf.compressed_file_size AS compressed_file_size, ",
        "gblf.md5 AS md5, ",
        "gblf.created_at AS created_at, ",
        "gblf.updated_at AS updated_at "
    >>.

common_joins() ->
    <<>>.
