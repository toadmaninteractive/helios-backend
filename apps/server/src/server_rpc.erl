-module(server_rpc).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").

%% Exported functions

-export([
    create_game/2,
    rename_game/2,
    publish_game/1,
    unpublish_game/1,
    enable_game/1,
    disable_game/1,
    set_jira_key/2,
    unset_jira_key/1,
    set_selene_key/2,
    unset_selene_key/1,
    set_discord_url/2,
    unset_discord_url/1,
    create_branch/3,
    create_branch/4,
    set_default_branch/2,
    set_public_branch/2,
    unset_public_branch/2,
    set_build_changelog/3,
    push_build/1
]).

%% API

-spec create_game(Guid, Title) -> Result when
    Guid :: file:filename_all(),
    Title :: file:filename_all(),
    Result :: 'ok' | atom().

create_game(Guid, Title) ->
    case db_if_games:create(h2s(Guid), h2s(Title), 0, <<"EUR">>) of
        {ok, _} -> ok;
        {error, Reason} -> Reason
    end.

-spec rename_game(Guid, Title) -> Result when
    Guid :: file:filename_all(),
    Title :: file:filename_all(),
    Result :: 'ok' | atom().

rename_game(Guid, Title) ->
    case db_if_games:update(h2s(Guid), #{<<"title">> => h2s(Title)}) of
        ok -> ok;
        {error, Reason} -> Reason
    end.

-spec publish_game(Guid) -> Result when
    Guid :: file:filename_all(),
    Result :: 'ok' | atom().

publish_game(Guid) ->
    case db_if_games:publish(h2s(Guid)) of
        ok -> ok;
        {error, Reason} -> Reason
    end.

-spec unpublish_game(Guid) -> Result when
    Guid :: file:filename_all(),
    Result :: 'ok' | atom().

unpublish_game(Guid) ->
    case db_if_games:unpublish(h2s(Guid)) of
        ok -> ok;
        {error, Reason} -> Reason
    end.

-spec enable_game(Guid) -> Result when
    Guid :: file:filename_all(),
    Result :: 'ok' | atom().

enable_game(Guid) ->
    case db_if_games:enable(h2s(Guid)) of
        ok -> ok;
        {error, Reason} -> Reason
    end.

-spec disable_game(Guid) -> Result when
    Guid :: file:filename_all(),
    Result :: 'ok' | atom().

disable_game(Guid) ->
    case db_if_games:disable(h2s(Guid)) of
        ok -> ok;
        {error, Reason} -> Reason
    end.

-spec set_jira_key(Guid, JiraKey) -> Result when
    Guid :: file:filename_all(),
    JiraKey :: file:filename_all(),
    Result :: 'ok' | atom().

set_jira_key(Guid, JiraKey) ->
    case db_if_games:set_jira_key(h2s(Guid), h2s(JiraKey)) of
        ok -> ok;
        {error, Reason} -> Reason
    end.

-spec unset_jira_key(Guid) -> Result when
    Guid :: file:filename_all(),
    Result :: 'ok' | atom().

unset_jira_key(Guid) ->
    case db_if_games:set_jira_key(h2s(Guid), ?null) of
        ok -> ok;
        {error, Reason} -> Reason
    end.

-spec set_selene_key(Guid, SeleneKey) -> Result when
    Guid :: file:filename_all(),
    SeleneKey :: file:filename_all(),
    Result :: 'ok' | atom().

set_selene_key(Guid, SeleneKey) ->
    case db_if_games:set_selene_key(h2s(Guid), h2s(SeleneKey)) of
        ok -> ok;
        {error, Reason} -> Reason
    end.

-spec unset_selene_key(Guid) -> Result when
    Guid :: file:filename_all(),
    Result :: 'ok' | atom().

unset_selene_key(Guid) ->
    case db_if_games:set_selene_key(h2s(Guid), ?null) of
        ok -> ok;
        {error, Reason} -> Reason
    end.

-spec set_discord_url(Guid, DiscordUrl) -> Result when
    Guid :: file:filename_all(),
    DiscordUrl :: file:filename_all(),
    Result :: 'ok' | atom().

set_discord_url(Guid, DiscordUrl) ->
    case db_if_games:set_discord_url(h2s(Guid), h2s(DiscordUrl)) of
        ok -> ok;
        {error, Reason} -> Reason
    end.

-spec unset_discord_url(Guid) -> Result when
    Guid :: file:filename_all(),
    Result :: 'ok' | atom().

unset_discord_url(Guid) ->
    case db_if_games:set_discord_url(h2s(Guid), ?null) of
        ok -> ok;
        {error, Reason} -> Reason
    end.

-spec create_branch(Guid, Branch, Password) -> Result when
    Guid :: file:filename_all(),
    Branch :: file:filename_all(),
    Password :: file:filename_all(),
    Result :: 'ok' | atom().

create_branch(Guid, Branch, Password) ->
    create_branch(Guid, Branch, Password, <<"windows">>).

-spec create_branch(Guid, Branch, Password, Platform) -> Result when
    Guid :: file:filename_all(),
    Branch :: file:filename_all(),
    Password :: file:filename_all(),
    Platform :: file:filename_all(),
    Result :: 'ok' | atom().

create_branch(Guid, Branch, Password, Platform) ->
    case db_if_game_branches:create(h2s(Guid), h2s(Branch), <<>>, h2s(Password), ?null, h2s(Platform)) of
        {ok, _} -> ok;
        {error, Reason} -> Reason
    end.

-spec set_default_branch(Guid, Branch) -> Result when
    Guid :: file:filename_all(),
    Branch :: file:filename_all(),
    Result :: 'ok' | atom().

set_default_branch(Guid, Branch) ->
    case db_if_game_branches:get_id_by_name(h2s(Guid), h2s(Branch)) of
        {ok, BranchId} ->
            case db_if_game_branches:set_default_branch(BranchId) of
                ok -> ok;
                {error, Reason} -> Reason
            end;
        {error, Reason} -> Reason
    end.

-spec set_public_branch(Guid, Branch) -> Result when
    Guid :: file:filename_all(),
    Branch :: file:filename_all(),
    Result :: 'ok' | atom().

set_public_branch(Guid, Branch) ->
    case db_if_game_branches:get_id_by_name(h2s(Guid), h2s(Branch)) of
        {ok, BranchId} ->
            case db_if_game_branches:set_public(BranchId, true) of
                ok -> ok;
                {error, Reason} -> Reason
            end;
        {error, Reason} -> Reason
    end.

-spec unset_public_branch(Guid, Branch) -> Result when
    Guid :: file:filename_all(),
    Branch :: file:filename_all(),
    Result :: 'ok' | atom().

unset_public_branch(Guid, Branch) ->
    case db_if_game_branches:get_id_by_name(h2s(Guid), h2s(Branch)) of
        {ok, BranchId} ->
            case db_if_game_branches:set_public(BranchId, false) of
                ok -> ok;
                {error, Reason} -> Reason
            end;
        {error, Reason} -> Reason
    end.

-spec set_build_changelog(Guid, BuildRev, ChangeLogPath) -> Result when
    Guid :: file:filename_all(),
    BuildRev :: file:filename_all(),
    ChangeLogPath :: file:filename_all(),
    Result :: 'ok' | atom().

set_build_changelog(Guid, BuildRev, ChangeLogPath) ->
    case db_if_game_builds:get_id_by_rev(Guid, BuildRev) of
        {ok, BuildId} ->
            Patch = #{<<"change_list">> => safe_contents(ChangeLogPath, <<>>)},
            case db_if_game_builds:update(BuildId, Patch) of
                ok -> ok;
                {error, Reason} -> Reason
            end;
        {error, Reason} -> Reason
    end.

-spec push_build(ManifestPath) -> Result when
    ManifestPath :: file:filename_all(),
    Result :: 'ok' | atom().

push_build(ManifestPath) ->
    try
        {ok, Guid, Branch, Build} = push_build_safe(h2s(ManifestPath)),
        logger:info("SUCCESS: pushed build ~s to branch ~s of game ~s~n", [Build, Branch, Guid], #{caption => ?MODULE}),
        ok
    catch
        throw:Error ->
            Error;
        Type:What:StackTrace ->
            logger:error("FAILURE: cannot push build from manifest: ~s (reason: ~p:~p)", [Type, What, ManifestPath], #{caption => ?MODULE, stacktrace => StackTrace}),
            crash
    end.

%% Local functions

h2s(String) when is_list(String) ->
    h2s(iolist_to_binary(String));
h2s(Binary) ->
    binary:replace(Binary, <<"####">>, <<" ">>, [global]).

safe_contents(FilePath, Default) ->
    try
        {ok, Contents} = file:read_file(FilePath),
        Contents
    catch
        _:_:_ -> Default
    end.

push_build_safe(ManifestPath) ->
    % Read manifest
    {ok, JsonStr} = file:read_file(ManifestPath),
    Json = jsx:decode(JsonStr, [return_maps]),
    CrashReportPath = maps:get(<<"crash_report_path">>, Json, <<>>),
    ConfigPath = maps:get(<<"config_path">>, Json, <<>>),
    OptionalFileMasks = case maps:get(<<"optional_file_masks">>, Json, []) of
        L when is_list(L) -> [Mask || Mask <- L, is_binary(Mask)];
        Mask when is_binary(Mask), Mask =/= <<>> -> [Mask];
        _ -> []
    end,
    PreservedFileMasks = case maps:get(<<"preserved_file_masks">>, Json, []) of
        L1 when is_list(L1) -> [Mask1 || Mask1 <- L1, is_binary(Mask1)];
        Mask1 when is_binary(Mask1), Mask1 =/= <<>> -> [Mask1];
        _ -> []
    end,
    Redistributables = case maps:get(<<"redistributables">>, Json, []) of
        L2 when is_list(L2) -> [web_protocol:redistributable_entry_from_json(R) || R <- L2];
        _ -> []
    end,
    PdbFiles = case maps:get(<<"pdb_files">>, Json, []) of
        L3 when is_list(L3) -> [File || File <- L3, is_binary(File)];
        _ -> []
    end,
    #{
        <<"guid">> := Guid,
        <<"branch">> := Branch,
        <<"build">> := BuildRev,
        <<"total_build_size">> := TotalSize,
        <<"total_compressed_size">> := CompressedSize,
        <<"cdn_root_url">> := CdnRootUrl,
        <<"exe_path">> := ExePath,
        <<"log_path">> := LogPath,
        <<"files">> := Files
    } = Json,

    % Check if game exists
    {ok, GameExists} = db_if_games:exists(Guid),
    ?doif(not GameExists, throw(game_not_exists)),

    % Check if game build already exists
    {ok, BuildExists} = db_if_game_builds:exists(Guid, BuildRev),
    ?doif(BuildExists, throw(build_already_exists)),

    % Add new build and manifest, set branch to this build
    {ok, NewBuildId} = db_if_game_builds:create(
        Guid,
        BuildRev,
        TotalSize,
        CompressedSize,
        ExePath,
        LogPath,
        CrashReportPath,
        ConfigPath,
        OptionalFileMasks,
        PreservedFileMasks,
        Redistributables,
        PdbFiles,
        CdnRootUrl
    ),

    ok = db_if_game_manifests:add(NewBuildId, #{files => Files}),

    % Check if build should not be put to a branch (detached)
    IsDetached = Branch =:= <<"no-branch">> orelse Branch =:= <<"nobranch">>,

    % Assign build to branch (if applicable)
    ok = ?yesno(IsDetached, ok, push_build_to_branch(Guid, Branch, NewBuildId)),

    {ok, Guid, Branch, BuildRev}.

push_build_to_branch(Guid, Branch, BuildId) ->
    % Check if game branch exists
    {ok, BranchExists} = db_if_game_branches:exists(Guid, Branch),
    ?doif(not BranchExists, throw(branch_not_exists)),
    {ok, BranchId} = db_if_game_branches:get_id_by_name(Guid, Branch),
    ok = db_if_game_branches:assign_build_to_branch(BranchId, BuildId),
    ok = db_if_game_branch_assignments:create(BranchId, BuildId),
    web_notify:game_item_updated(Guid),
    ok.
