-module(db_if_game_branches).

%% Include files

-include_lib("epgsql/include/epgsql.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get_one_by_name/2,
    get_id_by_name/2,
    get_ids_by_password/2,
    get/5,
    get_count/1,
    get_all/1,
    get_all_for_platform/2,
    create/6,
    update/2,
    update/3,
    delete/1,
    undelete/1,
    game_id/1,
    set_public/2,
    set_default_branch/1,
    assign_build_to_branch/2,
    exists/2
]).

%% API

-spec get_one(BranchId :: non_neg_integer()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one(BranchId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_branches AS gbr ",
        (common_joins())/binary,
        "WHERE gbr.id = $1"
    >>,
    case db_query:select_one(Query, [BranchId]) of
        {ok, Item} -> {ok, Item};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_one_by_name(Guid :: binary(), BranchName :: binary()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one_by_name(Guid, BranchName) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_branches AS gbr ",
        (common_joins())/binary,
        "WHERE gbr.game_id = TRIM($1) AND gbr.title = TRIM($2)"
    >>,
    case db_query:select_one(Query, [Guid, BranchName]) of
        {ok, Item} -> {ok, Item};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_id_by_name(Guid :: binary(), BranchName :: binary()) ->
    {'ok', non_neg_integer()} | {'error', Reason :: atom()}.

get_id_by_name(Guid, BranchName) ->
    Query = <<"SELECT id FROM game_branches WHERE game_id = TRIM($1) AND title = TRIM($2)">>,
    case db_query:select_one(Query, [Guid, BranchName]) of
        {ok, #{<<"id">> := BranchId}} -> {ok, BranchId};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_ids_by_password(Guid :: binary(), Password :: binary()) ->
    {'ok', [BranchId :: non_neg_integer()]} | {'error', Reason :: atom()}.

get_ids_by_password(Guid, Password) ->
    Query = <<"SELECT id FROM game_branches WHERE game_id = TRIM($1) AND password = $2">>,
    case db_query:select(Query, [Guid, Password]) of
        {ok, Items} -> {ok, [BranchId || #{<<"id">> := BranchId} <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec get(GameId :: non_neg_integer(), OrderBy :: binary(), OrderDir :: binary(), Offset :: non_neg_integer(), Limit :: non_neg_integer()) ->
    {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get(GameId, OrderBy, OrderDir, Offset, Limit) ->
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Statement = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_branches AS gbr ",
        (common_joins())/binary,
        "WHERE gbr.game_id = $1 ",
        Filter/binary
    >>,
    db_query:select(Statement, [GameId]).

-spec get_count(GameId :: non_neg_integer()) ->
    {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_count(GameId) ->
    Statement = <<"SELECT count(*)::bigint AS count FROM game_branches WHERE game_id = $1">>,
    case db_query:select_one(Statement, [GameId]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_all(GameId :: non_neg_integer()) ->
    {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_all(GameId) ->
    Statement = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_branches AS gbr ",
        (common_joins())/binary,
        "WHERE gbr.game_id = $1 ",
        "ORDER BY platform ASC, is_default DESC, is_public DESC, is_deleted ASC, title ASC"
    >>,
    db_query:select(Statement, [GameId]).

-spec get_all_for_platform(GameId :: non_neg_integer(), Platform :: web_protocol:platform()) ->
    {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_all_for_platform(GameId, Platform) ->
    Statement = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM game_branches AS gbr ",
        (common_joins())/binary,
        "WHERE gbr.game_id = $1 AND gbr.platform = $2 ",
        "ORDER BY platform ASC, is_default DESC, is_public DESC, is_deleted ASC, title ASC"
    >>,
    db_query:select(Statement, [GameId, web_protocol:platform_to_json(Platform)]).

-spec create(GameId, Title, Description, Password, GameEngine, Platform) -> Result when
    GameId :: non_neg_integer(),
    Title :: binary(),
    Description :: binary(),
    Password :: binary(),
    GameEngine :: binary() | 'null',
    Platform :: binary(),
    Result :: {'ok', BranchId :: non_neg_integer()} | {'error', Reason :: atom()}.

create(GameId, Title, Description, Password, GameEngine, Platform) ->
    Query = <<
        "INSERT INTO game_branches (game_id, title, description, password, game_engine, platform) ",
        "VALUES ($1, TRIM($2), TRIM($3), TRIM($4), $5, $6) ",
        "RETURNING id::bigint"
    >>,
    Params = [GameId, Title, Description, Password, GameEngine, Platform],
    case db_query:insert(Query, Params, [raw_error]) of
        {ok, 1, Columns, Rows} ->
            [#{<<"id">> := BranchId}] = db_util:result_to_json(Columns, Rows),
            {ok, BranchId};
        {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"game_branches_game_id_fkey">> -> {error, invalid_game_id}
            end;
        {error, #error{codename = check_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"game_branches_title_check">> ->  {error, invalid_branch_title}
            end;
        {error, #error{codename = unique_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"gbr_game_branch_uc_index">> -> {error, branch_title_already_exists}
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

-spec update(BranchId :: non_neg_integer(), Patch :: maps:map()) ->
    'ok' | {'error', Reason :: atom()}.

update(BranchId, Patch) ->
    update(BranchId, undefined, Patch).

-spec update(BranchId :: non_neg_integer(), Rev :: non_neg_integer() | 'undefined', Patch :: maps:map()) ->
    'ok' | {'error', Reason :: atom()}.

update(BranchId, Rev, Patch) ->
    % Define all possible fields
    Fields = [
        ?mk_mod_trim(title),
        ?mk_mod_trim(description),
        ?mk_mod_trim(password),
        ?mk_mod_trim(game_engine),
        ini_config,
        registry_config,
        is_reportable,
        is_public,
        is_deleted
    ],

    % Define auto-set fields
    AutoSetFields = [
        ?mk_mod_inc(rev),
        ?mk_mod_set_now(updated_at)
    ],

    % Perform update
    case db_util:mk_update(<<"game_branches">>, <<"id">>, BranchId, <<"rev">>, Rev, Fields, AutoSetFields, Patch) of
        {ok, Query, Params} ->
            case db_query:update(Query, Params, [raw_error]) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, not_exists};
                {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"game_branches_build_id_fkey">> -> {error, invalid_build_id}
                    end;
                {error, #error{codename = check_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"game_branches_title_check">> ->  {error, invalid_branch_title}
                    end;
                {error, #error{codename = unique_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"gbr_game_branch_uc_index">> -> {error, branch_title_already_exists}
                    end;
                {error, #error{codename = Code}} ->
                    {error, Code}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete(BranchId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

delete(BranchId) ->
    update(BranchId, #{<<"is_deleted">> => true}).

-spec undelete(BranchId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

undelete(BranchId) ->
    update(BranchId, #{<<"is_deleted">> => false}).

-spec game_id(BranchId :: non_neg_integer()) ->
    {'ok', binary()} | {'error', Reason :: atom()}.

game_id(BranchId) ->
    Query = <<"SELECT game_id FROM game_branches WHERE id = $1">>,
    case db_query:select_one(Query, [BranchId]) of
        {ok, #{<<"game_id">> := GameId}} -> {ok, GameId};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec set_public(BranchId :: non_neg_integer(), Value :: boolean()) ->
    'ok' | {'error', Reason :: atom()}.

set_public(BranchId, Value) ->
    update(BranchId, #{<<"is_public">> => Value =:= true}).

-spec set_default_branch(BranchId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

set_default_branch(BranchId) ->
    Query = <<"SELECT set_default_branch($1) AS result">>,
    case db_query:transaction({Query, [BranchId]}) of
        {ok, #{result := [#{<<"result">> := <<"ok">>}|_]}} -> ok;
        {ok, #{result := [#{<<"result">> := <<"branch_not_exists">>}|_]}} -> {error, branch_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec assign_build_to_branch(BranchId :: non_neg_integer(), BuildId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

assign_build_to_branch(BranchId, BuildId) ->
    Query = <<"SELECT assign_build_to_branch($1, $2) AS result">>,
    case db_query:transaction({Query, [BranchId, BuildId]}) of
        {ok, #{result := [#{<<"result">> := <<"ok">>}|_]}} -> ok;
        {ok, #{result := [#{<<"result">> := <<"branch_not_exists">>}|_]}} -> {error, branch_not_exists};
        {ok, #{result := [#{<<"result">> := <<"build_not_exists">>}|_]}} -> {error, build_not_exists};
        {ok, #{result := [#{<<"result">> := <<"game_mismatch">>}|_]}} -> {error, game_mismatch};
        {ok, #{result := [#{<<"result">> := <<"platform_mismatch">>}|_]}} -> {error, platform_mismatch};
        {error, Reason} -> {error, Reason}
    end.

-spec exists(Guid :: binary(), BranchName :: binary()) ->
    {'ok', boolean()} | {'error', Reason :: atom()}.

exists(Guid, BranchName) ->
    Statement = <<"SELECT count(*)::bigint AS count FROM game_branches WHERE game_id = $1 AND title = TRIM($2)">>,
    case db_query:select_one(Statement, [Guid, BranchName]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count > 0};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % gm : games
    % gbl : game_builds
    % gbr : game_branches
    <<
        "gbr.id AS id, ",
        "gbr.rev AS rev, ",
        "gbr.game_id AS game_id, ",
        "gm.title AS game_title, ",
        "gbr.title AS title, ",
        "gbr.description AS description, ",
        "gbr.password AS password, ",
        "gbr.build_id AS build_id, ",
        "gbl.build_rev AS build_rev, ",
        "gbl.commentary AS build_commentary, ",
        "gbl.change_list AS build_change_list, ",
        "gbl.total_size AS build_total_size, ",
        "gbl.compressed_size AS build_compressed_size, ",
        "gbl.exe_path AS build_exe_path, ",
        "gbl.log_path AS build_log_path, ",
        "gbl.crash_report_path AS build_crash_report_path, ",
        "gbl.config_path AS build_config_path, ",
        "gbl.optional_file_masks AS build_optional_file_masks, ",
        "gbl.preserved_file_masks AS build_preserved_file_masks, ",
        "gbl.redistributables AS build_redistributables, ",
        "gbl.pdb_files AS build_pdb_files, ",
        "gbl.cdn_root_url AS build_cdn_root_url, ",
        "gbl.created_at AS build_created_at, ",
        "gbr.game_engine AS game_engine, ",
        "gbr.platform AS platform, ",
        "gbr.ini_config AS ini_config, ",
        "gbr.registry_config AS registry_config, ",
        "gbr.is_reportable AS is_reportable, ",
        "gbr.is_public AS is_public, ",
        "gbr.is_default AS is_default, ",
        "gbr.is_deleted AS is_deleted, ",
        "gbr.created_at AS created_at, ",
        "gbr.updated_at AS updated_at "
    >>.

common_joins() ->
    <<
        "LEFT OUTER JOIN games AS gm ON (gbr.game_id = gm.id) ",
        "LEFT OUTER JOIN game_builds AS gbl ON (gbr.build_id = gbl.id) "
    >>.
