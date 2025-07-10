-module(web_rest_callback_ci).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("web_protocol.hrl").

%% Exported functions

-export([
    add_game_build/4
]).

%% API

-spec add_game_build(Request, Guid, Branch, ApiKey) -> Response when
    Request :: web_protocol:build_manifest(),
    Guid :: binary(),
    Branch :: binary() | 'undefined',
    ApiKey :: binary() | 'undefined',
    Response :: web_protocol:build().

add_game_build(Request, Guid, Branch, ApiKey) ->
    % Decompose build manifest
    #build_manifest{
        build_rev = BuildRev,
        cdn_root_url = CdnRootUrl,
        total_build_size = TotalSize,
        total_compressed_size = CompressedSize,
        exe_path = ExePath,
        log_path = LogPath,
        crash_report_path = CrashReportPath,
        config_path = ConfigPath,
        optional_file_masks = OptionalFileMasks,
        preserved_file_masks = PreservedFileMasks,
        redistributables = Redistributables,
        pdb_files = PdbFiles,
        platform = BuildPlatform
    } = Request,

    % Get actual CI API key
    {ok, CiApiKey} = db_if_settings:ci_api_key(),

    try
        % Check CI API key
        ?assert(is_binary(CiApiKey) andalso ApiKey =:= CiApiKey, invalid_api_key),

        % Check exe path
        ?assert(is_binary(ExePath) andalso ExePath =/= <<>>, invalid_exe_path),

        % Check if game exists
        case db_if_games:exists(Guid) of
            {ok, true} -> ignore;
            {ok, false} -> erlang:throw(invalid_game);
            {error, Reason} -> erlang:throw(Reason)
        end,

        % Check if branch platform matches build platform
        ?doif(is_binary(Branch), case db_if_game_branches:get_one_by_name(Guid, Branch) of
            {ok, #{<<"platform">> := Platform}} -> ?assert(web_protocol:platform_from_json(Platform) =:= BuildPlatform, platform_mismatch);
            {error, ?err_not_exists} -> erlang:throw(invalid_branch);
            {error, Reason1} -> erlang:throw(Reason1)
        end),

        % Check if build revision exists
        case db_if_game_builds:exists(Guid, BuildRev) of
            {ok, true} -> erlang:throw(build_already_exists);
            {ok, false} -> ignore;
            _ -> erlang:throw(failure)
        end,

        % Add build
        BuildId = case db_if_game_builds:create(Guid, BuildRev, TotalSize, CompressedSize, ExePath, LogPath, CrashReportPath, ConfigPath, OptionalFileMasks, PreservedFileMasks, Redistributables, PdbFiles, CdnRootUrl) of
            {ok, Id} -> Id;
            {error, ?err_already_exists} -> erlang:throw(build_already_exists);
            {error, Reason2} -> erlang:throw(Reason2)
        end,

        % Add manifest
        db_if_game_manifests:add(BuildId, maps:with([<<"files">>], web_protocol:build_manifest_to_json(Request))),

        % Assign build to branch if necessary
        ?doif(is_binary(Branch), begin
            {ok, BranchId} = db_if_game_branches:get_id_by_name(Guid, Branch),
            case db_if_game_branches:assign_build_to_branch(BranchId, BuildId) of
                ok ->
                    ok = db_if_game_branch_assignments:create(BranchId, BuildId),
                    ignore;
                {error, Reason3} ->
                    erlang:throw(Reason3)
            end
        end),

        % Send Websocket notification
        web_notify:game_item_updated(Guid),

        % Return newly added build
        case db_if_game_builds:get_one(BuildId) of
            {ok, BuildJson} -> web_protocol:build_from_json(BuildJson);
            {error, Reason4} -> erlang:throw(Reason4)
        end
    catch
        % 400
        throw:invalid_exe_path:_ -> web_rest_ci_builds_game:add_game_build_400(#bad_request_error{error = invalid_exe_path});
        throw:build_already_exists:_ -> web_rest_ci_builds_game:add_game_build_400(#bad_request_error{error = build_already_exists});
        throw:platform_mismatch:_ -> web_rest_ci_builds_game:add_game_build_400(#bad_request_error{error = platform_mismatch});

        % 403 (legacy)
        throw:invalid_api_key:_ -> web_rest_ci_builds_game:add_game_build_400(#bad_request_error{error = invalid_api_key});

        % 404 (legacy)
        throw:invalid_game:_ -> web_rest_ci_builds_game:add_game_build_400(#bad_request_error{error = invalid_game});
        throw:invalid_branch:_ -> web_rest_ci_builds_game:add_game_build_400(#bad_request_error{error = invalid_branch});

        % 500
        Type:What:StackTrace ->
            logger:error("Failed to add game build (reason: ~p:~p)", [Type, What], #{caption => ?MODULE, stacktrace => StackTrace}),
            web_rest_ci_builds_game:add_game_build_500(#internal_server_error{error = What})
    end.

%% Local functions
