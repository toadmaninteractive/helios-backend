-module(db_if_game_builds).

%% Include files

-include_lib("epgsql/include/epgsql.hrl").
-include_lib("aplib/include/apmacros.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get_one_by_rev/2,
    get_id_by_rev/2,
    get/5,
    get_count/1,
    get_for_employee/5,
    get_for_employee_count/1,
    get_for_game/6,
    get_for_game_count/2,
    get_for_branch/3,
    get_for_branch_count/1,
    get_for_game_employee/8,
    get_for_game_employee_count/4,
    get_expired/0,
    get_change_logs/5,
    get_change_logs_count/3,
    create/13,
    create/14,
    create_draft/4,
    publish_draft/1,
    update/2,
    update/3,
    delete/1,
    undelete/1,
    wipe/1,
    exists/2,
    has_assigned_branches/1,
    game_id/1,
    inc_processed_size/2
]).

%% API

-spec get_one(BuildId :: non_neg_integer()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one(BuildId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_builds AS gbl ",
        (common_joins())/binary,
        "WHERE gbl.id = $1 ",
        "GROUP BY (gbl.id, gm.title) "
    >>,
    case db_query:select_one(Query, [BuildId]) of
        {ok, Item} -> {ok, Item};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_one_by_rev(GameId :: binary(), BuildRev :: binary()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one_by_rev(GameId, BuildRev) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_builds AS gbl ",
        (common_joins())/binary,
        "WHERE gbl.game_id = $1 AND gbl.build_rev = $2 ",
        "GROUP BY (gbl.id, gm.title) "
    >>,
    case db_query:select_one(Query, [GameId, BuildRev]) of
        {ok, Item} -> {ok, Item};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_id_by_rev(GameId :: binary(), BuildRev :: binary()) ->
    {'ok', non_neg_integer()} | {'error', Reason :: atom()}.

get_id_by_rev(GameId, BuildRev) ->
    Query = <<"SELECT id FROM game_builds WHERE game_id = $1 AND build_rev = $2">>,
    case db_query:select_one(Query, [GameId, BuildRev]) of
        {ok, #{<<"id">> := BuildId}} -> {ok, BuildId};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get(OrderBy, OrderDir, Offset, Limit, ActiveOnly) -> Result when
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    ActiveOnly :: boolean() | 'undefined',
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get(OrderBy, OrderDir, Offset, Limit, ActiveOnly) ->
    ActiveFilter = ?yesno(is_boolean(ActiveOnly), <<"WHERE gbl.is_deleted <> $1 ">>, <<>>),
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Statement = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_builds AS gbl ",
        (common_joins())/binary,
        ActiveFilter/binary,
        "GROUP BY (gbl.id, gm.title) ",
        Filter/binary
    >>,
    Params = ?yesno(is_boolean(ActiveOnly), [ActiveOnly], []),
    db_query:select(Statement, Params).

-spec get_count(ActiveOnly :: boolean() | 'undefined') ->
    {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_count(ActiveOnly) ->
    ActiveFilter = ?yesno(is_boolean(ActiveOnly), <<"WHERE is_deleted <> $1 ">>, <<>>),
    Statement = <<
        "SELECT count(*)::bigint AS count FROM game_builds ",
        ActiveFilter/binary
    >>,
    Params = ?yesno(is_boolean(ActiveOnly), [ActiveOnly], []),
    case db_query:select_one(Statement, Params) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_for_employee(UserId, OrderBy, OrderDir, Offset, Limit) -> Result when
    UserId :: non_neg_integer(),
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_for_employee(UserId, OrderBy, OrderDir, Offset, Limit) ->
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Statement = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_builds AS gbl ",
        (common_joins())/binary,
        "LEFT OUTER JOIN personnel_roles AS puacl ON (puacl.game_id = gbl.game_id AND puacl.personnel_id = $1) ",
        "WHERE is_game_manager(gbl.game_id, $1) ",
        "GROUP BY (gbl.id, gm.title) ",
        Filter/binary
    >>,
    db_query:select(Statement, [UserId]).

-spec get_for_employee_count(UserId) -> Result when
    UserId :: non_neg_integer(),
    Result :: {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_for_employee_count(UserId) ->
    Statement = <<
        "SELECT COUNT(gbl.*)::bigint AS count ",
        "FROM game_builds AS gbl ",
        "WHERE is_game_manager(gbl.game_id, $1) "
    >>,
    case db_query:select_one(Statement, [UserId]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_for_game(GameId, Platform, OrderBy, OrderDir, Offset, Limit) -> Result when
    GameId :: binary(),
    Platform :: web_protocol:platform(),
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_for_game(GameId, Platform, OrderBy, OrderDir, Offset, Limit) ->
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    PlarformSql = ?yesno(Platform =:= undefined, <<>>, <<" AND gbl.platform = $2 ">>),
    Statement = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_builds AS gbl ",
        (common_joins())/binary,
        "WHERE gbl.game_id = $1 ",
        PlarformSql/binary,
        "GROUP BY (gbl.id, gm.title) ",
        Filter/binary
    >>,
    Params = [GameId] ++ ?yesno(Platform =:= undefined, [], [web_protocol:platform_to_json(Platform)]),
    db_query:select(Statement, Params).

-spec get_for_game_count(GameId :: binary(), Platform :: web_protocol:platform()) ->
    {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_for_game_count(GameId, Platform) ->
    PlarformSql = ?yesno(Platform =:= undefined, <<>>, <<" AND platform = $2 ">>),
    Statement = <<"SELECT count(*)::bigint AS count FROM game_builds WHERE game_id = $1 ", PlarformSql/binary>>,
    Params = [GameId] ++ ?yesno(Platform =:= undefined, [], [web_protocol:platform_to_json(Platform)]),
    case db_query:select_one(Statement, Params) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_for_branch(BranchId, Offset, Limit) -> Result when
    BranchId :: non_neg_integer(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_for_branch(BranchId, Offset, Limit) ->
    Filter = db_util:mk_query_filter(<<"gbra.assigned_at">>, <<"desc">>, Offset, Limit),
    Statement = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_branch_assignments AS gbra ",
        "LEFT OUTER JOIN game_builds AS gbl ON (gbra.build_id = gbl.id)",
        (common_joins())/binary,
        "WHERE gbra.branch_id = $1 ",
        "GROUP BY (gbl.id, gm.title, gbra.assigned_at) ",
        Filter/binary
    >>,
    db_query:select(Statement, [BranchId]).

-spec get_for_branch_count(BranchId) -> Result when
    BranchId :: non_neg_integer(),
    Result :: {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_for_branch_count(BranchId) ->
    Statement = <<
        "SELECT COUNT(gbra.*)::bigint AS count ",
        "FROM game_branch_assignments AS gbra ",
        "WHERE gbra.branch_id = $1 "
    >>,
    case db_query:select_one(Statement, [BranchId]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_for_game_employee(GameId, Platform, UserId, HasAccess, OrderBy, OrderDir, Offset, Limit) -> Result when
    GameId :: binary(),
    Platform :: web_protocol:platform(),
    UserId :: non_neg_integer(),
    HasAccess :: boolean(),
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_for_game_employee(GameId, Platform, UserId, HasAccess, OrderBy, OrderDir, Offset, Limit) ->
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Statement = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_builds AS gbl ",
        (common_joins())/binary,
        "LEFT OUTER JOIN personnel_user_acl AS puacl ON (puacl.game_id = gbl.game_id AND puacl.personnel_id = $3) ",
        "WHERE gbl.game_id = $1 AND gbl.platform = $2 AND (",
            "$4::boolean OR puacl.is_project_wide OR puacl.branch_ids @> (gbr.id::text)::jsonb",
        ")",
        "GROUP BY (gbl.id, gm.title) ",
        Filter/binary
    >>,
    Params = [GameId, web_protocol:platform_to_json(Platform), UserId, HasAccess],
    db_query:select(Statement, Params).

-spec get_for_game_employee_count(GameId, Platform, UserId, HasAccess) -> Result when
    GameId :: binary(),
    Platform :: web_protocol:platform(),
    UserId :: non_neg_integer(),
    HasAccess :: boolean(),
    Result :: {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_for_game_employee_count(GameId, Platform, UserId, HasAccess) ->
    Statement = <<
        "SELECT COUNT(gbl.*)::bigint AS count ",
        "FROM game_builds AS gbl ",
        "LEFT OUTER JOIN game_branches AS gbr ON (gbl.id = gbr.build_id) ",
        "LEFT OUTER JOIN personnel_user_acl AS puacl ON (puacl.game_id = gbl.game_id AND puacl.personnel_id = $3) ",
        "WHERE gbl.game_id = $1 AND gbl.platform = $2 AND (",
            "$4::boolean OR puacl.is_project_wide OR puacl.branch_ids @> (gbr.id::text)::jsonb",
        ")"
    >>,
    Params = [GameId, web_protocol:platform_to_json(Platform), UserId, HasAccess],
    case db_query:select_one(Statement, Params) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_expired() ->
    {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_expired() ->
    Statement = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_builds AS gbl ",
        (common_joins())/binary,
        "WHERE ",
            "gm.build_lifetime > 0 ",
            "AND gbr.id IS NULL ",
            "AND NOT gbl.is_permanent ",
            "AND EXTRACT(DAY FROM current_timestamp - gbl.created_at) > gm.build_lifetime ",
        "GROUP BY (gbl.id, gm.title)"
    >>,
    db_query:select(Statement, []).

-spec get_change_logs(GameId, BuildId, Search, Offset, Limit) -> Result when
    GameId :: binary(),
    BuildId :: non_neg_integer(),
    Search :: binary() | 'undefined',
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_change_logs(GameId, BuildId, Search, Offset, Limit) ->
    OrderBy = <<"build_created_at">>,
    OrderDir = <<"desc">>,
    IncludeSearch = is_binary(Search) andalso Search =/= <<>>,
    SearchPart = ?yesno(IncludeSearch, <<" AND strpos(LOWER(gbl.change_list), LOWER($3)) > 0 ">>, <<>>),
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Statement = <<
        "SELECT ",
            "gbl.build_rev AS build_rev, ",
            "gbl.change_list AS build_change_list, ",
            "gbl.created_at AS build_created_at ",
        "FROM game_builds AS gbl ",
        "WHERE gbl.game_id = $1 AND gbl.id <= $2 AND gbl.change_list <> '' ",
        SearchPart/binary,
        Filter/binary
    >>,
    Params = [GameId, BuildId] ++ ?yesno(IncludeSearch, [Search], []),
    db_query:select(Statement, Params).

-spec get_change_logs_count(GameId, BuildId, Search) -> Result when
    GameId :: binary(),
    BuildId :: non_neg_integer(),
    Search :: binary() | 'undefined',
    Result :: {'ok', non_neg_integer()} | {'error', Reason :: atom()}.

get_change_logs_count(GameId, BuildId, Search) ->
    IncludeSearch = is_binary(Search) andalso Search =/= <<>>,
    SearchPart = ?yesno(IncludeSearch, <<" AND strpos(LOWER(change_list), LOWER($3)) > 0 ">>, <<>>),
    Statement = <<
        "SELECT COUNT(*) AS count FROM game_builds ",
        "WHERE game_id = $1 AND id <= $2 AND change_list <> '' ",
        SearchPart/binary
    >>,
    Params = [GameId, BuildId] ++ ?yesno(IncludeSearch, [Search], []),
    case db_query:select_one(Statement, Params) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec create(GameId, BuildRev, TotalSize, CompressedSize, ExePath, LogPath, CrashReportPath, ConfigPath, OptionalFileMasks, PreservedFileMasks, Redistributables, PdbFiles, CdnRootUrl) -> Result when
    GameId :: binary(),
    BuildRev :: binary(),
    TotalSize :: non_neg_integer(),
    CompressedSize :: non_neg_integer(),
    ExePath :: binary(),
    LogPath :: binary(),
    CrashReportPath :: binary(),
    ConfigPath :: binary(),
    OptionalFileMasks :: [binary()],
    PreservedFileMasks :: [binary()],
    Redistributables :: [web_protocol:redistributable_entry()],
    PdbFiles :: [binary()],
    CdnRootUrl :: binary(),
    Result :: {'ok', BuildId :: non_neg_integer()} | {'error', Reason :: atom()}.

create(GameId, BuildRev, TotalSize, CompressedSize, ExePath, LogPath, CrashReportPath, ConfigPath, OptionalFileMasks, PreservedFileMasks, Redistributables, PdbFiles, CdnRootUrl) ->
    create(GameId, BuildRev, TotalSize, CompressedSize, ExePath, LogPath, CrashReportPath, ConfigPath, OptionalFileMasks, PreservedFileMasks, Redistributables, PdbFiles, CdnRootUrl, windows).

-spec create(GameId, BuildRev, TotalSize, CompressedSize, ExePath, LogPath, CrashReportPath, ConfigPath, OptionalFileMasks, PreservedFileMasks, Redistributables, PdbFiles, CdnRootUrl, Platform) -> Result when
    GameId :: binary(),
    BuildRev :: binary(),
    TotalSize :: non_neg_integer(),
    CompressedSize :: non_neg_integer(),
    ExePath :: binary(),
    LogPath :: binary(),
    CrashReportPath :: binary(),
    ConfigPath :: binary(),
    OptionalFileMasks :: [binary()],
    PreservedFileMasks :: [binary()],
    Redistributables :: [web_protocol:redistributable_entry()],
    PdbFiles :: [binary()],
    CdnRootUrl :: binary(),
    Platform :: web_protocol:platform(),
    Result :: {'ok', BuildId :: non_neg_integer()} | {'error', Reason :: atom()}.

create(GameId, BuildRev, TotalSize, CompressedSize, ExePath, LogPath, CrashReportPath, ConfigPath, OptionalFileMasks, PreservedFileMasks, Redistributables, PdbFiles, CdnRootUrl, Platform) ->
    Query = <<
        "INSERT INTO game_builds (",
            "game_id, ",
            "build_rev, ",
            "total_size, ",
            "compressed_size, ",
            "exe_path, ",
            "log_path, ",
            "crash_report_path, ",
            "config_path, ",
            "optional_file_masks, ",
            "preserved_file_masks, ",
            "redistributables, ",
            "pdb_files, ",
            "cdn_root_url, ",
            "platform",
        ") ",
        "VALUES ($1, TRIM($2), $3, $4, TRIM($5), TRIM($6), TRIM($7), TRIM($8), $9, $10, $11, $12, TRIM($13), $14) ",
        "RETURNING id::bigint"
    >>,
    Params = [
        GameId,
        BuildRev,
        TotalSize,
        CompressedSize,
        ExePath,
        LogPath,
        CrashReportPath,
        ConfigPath,
        jsx:encode(OptionalFileMasks),
        jsx:encode(PreservedFileMasks),
        jsx:encode([web_protocol:redistributable_entry_to_json(R) || R <- Redistributables]),
        jsx:encode(PdbFiles),
        CdnRootUrl,
        web_protocol:platform_to_json(Platform)
    ],
    case db_query:insert(Query, Params) of
        {ok, 1, Columns, Rows} ->
            [#{<<"id">> := BuildId}] = db_util:result_to_json(Columns, Rows),
            {ok, BuildId};
        {error, unique_violation} ->
            {error, ?err_already_exists};
        {error, Other} ->
            {error, Other}
    end.

-spec create_draft(GameId, BuildRev, CdnRootUrl, Platform) -> Result when
    GameId :: binary(),
    BuildRev :: binary(),
    CdnRootUrl :: binary(),
    Platform :: web_protocol:platform(),
    Result :: {'ok', BuildId :: non_neg_integer()} | {'error', Reason :: atom()}.

create_draft(GameId, BuildRev, CdnRootUrl, Platform) ->
    Query = <<
        "INSERT INTO game_builds (game_id, build_rev, total_size, compressed_size, cdn_root_url, platform, is_draft) ",
        "VALUES ($1, TRIM($2), 0, 0, TRIM($3), $4, TRUE) ",
        "RETURNING id::bigint"
    >>,
    Params = [
        GameId,
        BuildRev,
        CdnRootUrl,
        web_protocol:platform_to_json(Platform)
    ],
    case db_query:insert(Query, Params, [raw_error]) of
        {ok, 1, Columns, Rows} ->
            [#{<<"id">> := BuildId}] = db_util:result_to_json(Columns, Rows),
            {ok, BuildId};
        {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"game_builds_game_id_fkey">> -> {error, invalid_game_id}
            end;
        {error, #error{codename = check_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"game_builds_build_rev_check">> ->  {error, invalid_build_rev};
                <<"game_builds_cdn_root_url_check">> ->  {error, invalid_cdn_root_url}
            end;
        {error, #error{codename = unique_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"gbl_game_build_uc_index">> -> {error, build_rev_already_exists}
            end;
        {error, #error{codename = invalid_text_representation, message = Message}} ->
            InvalidPlatform = re:run(Message, <<"(platform_t)">>) =/= nomatch,
            if
                InvalidPlatform -> {error, invalid_platform};
                true -> {error, Message}
            end;
        {error, #error{codename = Code}} ->
            {error, Code}
    end.

-spec publish_draft(BuildId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

publish_draft(BuildId) ->
    Query = <<"SELECT publish_draft_build($1) AS result">>,
    case db_query:transaction({Query, [BuildId]}) of
        {ok, #{result := [#{<<"result">> := <<"ok">>}|_]}} -> ok;
        {ok, #{result := [#{<<"result">> := <<"build_not_exists">>}|_]}} -> {error, build_not_exists};
        {ok, #{result := [#{<<"result">> := <<"already_published">>}|_]}} -> {error, already_published};
        {ok, #{result := [#{<<"result">> := <<"no_files">>}|_]}} -> {error, no_files};
        {ok, #{result := [#{<<"result">> := <<"invalid_exe_path">>}|_]}} -> {error, invalid_exe_path};
        {error, Reason} -> {error, Reason}
    end.

-spec update(BuildId :: non_neg_integer(), Patch :: maps:map()) ->
    'ok' | {'error', Reason :: atom()}.

update(BuildId, Patch) ->
    update(BuildId, undefined, Patch).

-spec update(BuildId :: non_neg_integer(), Rev :: non_neg_integer() | 'undefined', Patch :: maps:map()) ->
    'ok' | {'error', Reason :: atom()}.

update(BuildId, Rev, Patch) ->
    % Define all possible fields
    Fields = [
        ?mk_mod_trim(commentary),
        ?mk_mod_trim(change_list),
        ?mk_mod_trim(cdn_root_url),
        ?mk_mod_trim(exe_path),
        ?mk_mod_trim(log_path),
        ?mk_mod_trim(crash_report_path),
        ?mk_mod_trim(config_path),
        optional_file_masks,
        preserved_file_masks,
        redistributables,
        pdb_files,
        platform,
        is_permanent,
        is_processing,
        archived_size,
        processed_size,
        is_deleted
    ],

    % Define auto-set fields
    AutoSetFields = [
        ?mk_mod_inc(rev),
        ?mk_mod_set_now(updated_at)
    ],

    % Perform update
    case db_util:mk_update(<<"game_builds">>, <<"id">>, BuildId, <<"rev">>, Rev, Fields, AutoSetFields, Patch) of
        {ok, Query, Params} ->
            case db_query:update(Query, Params) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, ?err_not_exists};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete(BuildId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

delete(BuildId) ->
    update(BuildId, #{<<"is_deleted">> => true}).

-spec undelete(BuildId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

undelete(BuildId) ->
    update(BuildId, #{<<"is_deleted">> => false}).

-spec wipe(BuildId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

wipe(BuildId) ->
    Query = <<"DELETE FROM game_builds WHERE id = $1">>,
    case db_query:delete(Query, [BuildId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec exists(Guid :: binary(), BuildRev :: binary()) ->
    {'ok', boolean()} | {'error', Reason :: atom()}.

exists(Guid, BuildRev) ->
    Statement = <<"SELECT count(*)::bigint AS count FROM game_builds WHERE game_id = trim($1) AND build_rev = trim($2)">>,
    case db_query:select_one(Statement, [Guid, BuildRev]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count > 0};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec has_assigned_branches(BuildId :: non_neg_integer()) ->
    {'ok', boolean()} | {'error', Reason :: atom()}.

has_assigned_branches(BuildId) ->
    Statement = <<"SELECT count(*)::bigint AS count FROM game_branches WHERE build_id = $1">>,
    case db_query:select_one(Statement, [BuildId]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count > 0};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec game_id(BuildId :: non_neg_integer()) ->
    {'ok', binary()} | {'error', Reason :: atom()}.

game_id(BuildId) ->
    Query = <<"SELECT game_id FROM game_builds WHERE id = $1">>,
    case db_query:select_one(Query, [BuildId]) of
        {ok, #{<<"game_id">> := GameId}} -> {ok, GameId};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec inc_processed_size(BuildId :: non_neg_integer(), Size :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

inc_processed_size(BuildId, Size) ->
    Query = <<
        "UPDATE game_builds ",
        "SET ",
            "processed_size = processed_size + $2, ",
            "rev = rev + 1, ",
            "updated_at = NOW() ",
        "WHERE id = $1 "
    >>,
    Params = [BuildId, Size],
    case db_query:update(Query, Params) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % gm : games
    % gbl : game_builds
    % gbr : game_branches
    <<
        "gbl.id AS id, ",
        "gbl.rev AS rev, ",
        "gbl.game_id AS game_id, ",
        "gm.title AS game_title, ",
        "CASE jsonb_agg(gbr.title)::jsonb WHEN '[null]'::jsonb THEN '[]'::jsonb ELSE jsonb_agg(gbr.title)::jsonb END AS branch_tags, ",
        "gbl.build_rev AS build_rev, ",
        "gbl.commentary AS commentary, ",
        "gbl.change_list AS change_list, ",
        "gbl.total_size AS total_size, ",
        "gbl.compressed_size AS compressed_size, ",
        "gbl.exe_path AS exe_path, ",
        "gbl.log_path AS log_path, ",
        "gbl.crash_report_path AS crash_report_path, ",
        "gbl.config_path AS config_path, ",
        "gbl.optional_file_masks AS optional_file_masks, ",
        "gbl.preserved_file_masks AS preserved_file_masks, ",
        "gbl.redistributables AS redistributables, ",
        "gbl.pdb_files AS pdb_files, ",
        "gbl.cdn_root_url AS cdn_root_url, ",
        "gbl.platform AS platform, ",
        "gbl.is_local AS is_local, ",
        "gbl.is_permanent AS is_permanent, ",
        "gbl.is_draft AS is_draft, ",
        "gbl.is_processing AS is_processing, ",
        "gbl.archived_size AS archived_size, ",
        "gbl.processed_size AS processed_size, ",
        "gbl.is_deleted AS is_deleted, ",
        "gbl.created_at AS created_at, ",
        "gbl.updated_at AS updated_at "
    >>.

common_joins() ->
    <<
        "LEFT OUTER JOIN games AS gm ON (gbl.game_id = gm.id) ",
        "LEFT OUTER JOIN game_branches AS gbr ON (gbl.id = gbr.build_id) "
    >>.
