-module(web_mobile_build_upload).

-behaviour(cowboy_handler).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("http.hrl").
-include("session.hrl").
-include("web_protocol.hrl").

%% Exported functions

-export([
    init/2
]).

-define(chunk_size, 16 * 1024 * 1024).
-define(execOrThrow(Statement, ThrowArg), try (Statement) catch _:_:_ -> erlang:throw(ThrowArg) end).

-record(upload_file, {
    filename :: filelib:filename_all(),
    content_type :: binary(),
    tmp_path :: binary(),
    chunk_size = ?chunk_size :: non_neg_integer(),
    recv_size = 0 :: non_neg_integer(),
    max_size = ?chunk_size :: non_neg_integer(),
    md5_state :: crypto:hash_state(),
    md5 :: binary()
}).

-record(upload_state, {
    params = #{} :: maps:map(Param :: binary(), Value :: binary()),
    files = #{} :: maps:map(Param :: binary(), #upload_file{}),
    uploads_dir :: filelib:filename_all(),
    chunk_size = ?chunk_size :: non_neg_integer(),
    max_upload_size = ?chunk_size :: non_neg_integer(),
    recv_size = 0 :: non_neg_integer()
}).

%% API

init(Req, Opts) ->
    % Get configuration data for uploads
    {ok, MaxUploadSize} = web_config:max_upload_size(),
    {ok, UploadsDir} = web_config:uploads_dir(),
    {ok, CdnRootUrl} = web_config:cdn_root_url(),
    {ok, WwwRoot} = web_config:www_root(),
    {ok, CiApiKey} = db_if_settings:ci_api_key(),

    % Read query parameters
    Guid = cowboy_req:binding(guid, Req),
    Qs = maps:from_list(cowboy_req:parse_qs(Req)),
    Branch = maps:get(<<"branch">>, Qs, undefined),
    ApiKey = cowboy_req:header(<<"x-api-key">>, Req),

    Req1 = try
        % Check CI API key
        ?assert(is_binary(CiApiKey) andalso ApiKey =:= CiApiKey, invalid_api_key),

        % Read multipart request
        InitialUploadState = #upload_state{chunk_size = ?chunk_size, max_upload_size = MaxUploadSize, uploads_dir = UploadsDir},
        {R1, #upload_state{files = Files, params = Params}} = read_multipart(Req, InitialUploadState),

        % Extract build informaton
        BuildRev = maps:get(<<"build_rev">>, Params, undefined),
        RootUrl = <<CdnRootUrl/binary, Guid/binary, "/", BuildRev/binary, "/">>,
        ?assert(is_binary(BuildRev) andalso BuildRev =/= <<>>, build_rev_not_set),
        BuildPlatform = ?execOrThrow(web_protocol:platform_from_json(maps:get(<<"platform">>, Params)), platform_not_set),
        #upload_file{filename = ExePath, tmp_path = TmpPath, md5 = Md5} = ?execOrThrow(#upload_file{} = maps:get(<<"build_file">>, Files), invalid_filename),

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
            {ok, #{<<"platform">> := Platform}} -> ?assert((web_protocol:platform_from_json(Platform) =:= BuildPlatform), platform_mismatch);
            {error, ?err_not_exists} -> erlang:throw(invalid_branch);
            {error, Reason1} -> erlang:throw(Reason1)
        end),

        % Check if build revision exists
        case db_if_game_builds:exists(Guid, BuildRev) of
            {ok, true} -> erlang:throw(build_already_exists);
            {ok, false} -> ignore;
            _ -> erlang:throw(failure)
        end,

        % Define file information
        FileSize = filelib:file_size(TmpPath),

        % TODO: make a transaction
        % Add build
        BuildId = case db_if_game_builds:create(Guid, BuildRev, FileSize, FileSize, ExePath, <<>>, <<>>, <<>>, [], [], [], [], RootUrl, BuildPlatform) of
            {ok, Id} -> Id;
            {error, ?err_already_exists} -> erlang:throw(build_already_exists);
            {error, Reason2} -> erlang:throw(Reason2)
        end,

        % Add manifest
        File = #game_file{relative_path = ExePath, relative_compressed_path = ExePath, size = FileSize, compressed_size = FileSize, md5 = Md5},
        Manifest = #{files => [web_protocol:game_file_to_json(File)]},
        db_if_game_manifests:add(BuildId, web_util:encode_json(Manifest)),

        % Move build file
        TargetPath = filename:join([WwwRoot, Guid, BuildRev, ExePath]),
        filelib:ensure_dir(TargetPath),
        file:rename(TmpPath, TargetPath),

        % Delete all temporary files
        maps:fold(fun(_, #upload_file{tmp_path = TP}, _) -> file:delete(TP) end, [], Files),

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

        % Return newly added build
        case db_if_game_builds:get_one(BuildId) of
            {ok, BuildJson} -> success(R1, BuildJson);
            {error, Reason4} -> erlang:throw(Reason4)
        end
    catch
        % 400
        throw:build_rev_not_set:_ -> bad_request(Req, build_rev_not_set);
        throw:invalid_filename:_ -> bad_request(Req, invalid_filename);
        throw:invalid_exe_path:_ -> bad_request(Req, invalid_exe_path);
        throw:build_already_exists:_ -> bad_request(Req, build_already_exists);
        throw:platform_not_set:_ -> bad_request(Req, platform_not_set);
        throw:platform_mismatch:_ -> bad_request(Req, platform_mismatch);

        % 403
        throw:invalid_api_key:_ -> forbidden(Req, invalid_api_key);

        % 404
        throw:invalid_game:_ -> not_found(Req, invalid_game);
        throw:invalid_branch:_ -> not_found(Req, invalid_branch);

        % 500
        Type:What:StackTrace ->
            logger:error("Failed to upload mobile game build (reason: ~p:~p)", [Type, What], #{caption => ?MODULE, stacktrace => StackTrace}),
            internal_server_error(Req, What)
    end,
    {ok, Req1, Opts}.

%% Local functions

tmp_filepath(PathPrefix) ->
    Uuid = util_hex:from_binary(uuid:get_v4(strong)),
    filename:join([PathPrefix, Uuid]).

read_multipart(Req, #upload_state{uploads_dir = UploadsDir, chunk_size = ChunkSize, max_upload_size = MaxSize, recv_size = RecvSize, files = Files, params = Params} = State) ->
    case cowboy_req:read_part(Req, #{length => MaxSize}) of
        {ok, Headers, Req1} ->
            case cow_multipart:form_data(Headers) of
                {data, Param} ->
                    {ok, Value, R1} = cowboy_req:read_part_body(Req1),
                    read_multipart(R1, State#upload_state{params = Params#{Param => Value}});
                {file, Param, FileName, ContentType} ->
                    TmpFilePath = tmp_filepath(UploadsDir),
                    file:delete(TmpFilePath),
                    InitialState = #upload_file{
                        filename = FileName,
                        content_type = ContentType,
                        tmp_path = TmpFilePath,
                        chunk_size = ChunkSize,
                        max_size = MaxSize,
                        md5_state = crypto:hash_init(md5),
                        md5 = <<>>
                    },
                    {R1, #upload_file{recv_size = RecvFileSize} = F} = recv_file(Req1, InitialState),
                    read_multipart(R1, State#upload_state{files = Files#{Param => F}, recv_size = RecvSize + RecvFileSize})
            end;
        {done, Req1} ->
            {Req1, State}
    end.

recv_file(Req, #upload_file{recv_size = RecvSize, max_size = _MaxSize, tmp_path = TmpPath, md5_state = Md5State} = State) ->
    case cowboy_req:read_part_body(Req, #{length => ?chunk_size, period => 7200}) of
        {ok, Data, Req1} ->
            write_file_chunk(TmpPath, Data),
            Md5State1 = crypto:hash_update(Md5State, Data),
            Md5 = util_hex:from_binary(crypto:hash_final(Md5State1)),
            {Req1, State#upload_file{recv_size = RecvSize + byte_size(Data), md5 = Md5, md5_state = undefined}};
        {more, Data, Req1} ->
            write_file_chunk(TmpPath, Data),
            Md5State1 = crypto:hash_update(Md5State, Data),
            recv_file(Req1, State#upload_file{recv_size = RecvSize + byte_size(Data), md5_state = Md5State1})
    end.

write_file_chunk(FilePath, Data) ->
    {ok, IoDevice} = file:open(FilePath, [append, raw, binary]),
    ok = file:write(IoDevice, Data),
    ok = file:close(IoDevice).

success(Req, Json) ->
    web_util:reply_json_explicit(?http_ok, Json, Req).

bad_request(Req, Error) ->
    web_util:reply_json_explicit(?http_bad_request, #{error => Error}, Req).

forbidden(Req, Error) ->
    web_util:reply_json_explicit(?http_forbidden, #{error => Error}, Req).

not_found(Req, Error) ->
    web_util:reply_json_explicit(?http_not_found, #{error => Error}, Req).

internal_server_error(Req, Error) ->
    web_util:reply_json_explicit(?http_internal_server_error, #{error => Error}, Req).
