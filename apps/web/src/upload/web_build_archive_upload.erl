-module(web_build_archive_upload).

-behaviour(cowboy_handler).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("eunzip/include/eunzip.hrl").
-include_lib("db/include/protocol.hrl").
-include("http.hrl").
-include("session.hrl").

%% Exported functions

-export([
    init/2
]).

-define(chunk_size, 32 * 1024 * 1024).  % 32 MB

-record(unpack, {
    build_id = 0 :: non_neg_integer(),
    file_handle :: file:fd(),
    total_size = 0 :: non_neg_integer(),
    processed_size = 0 :: non_neg_integer(),
    md5 :: binary(),
    z_stream :: reference()
}).

%% API

init(#{method := ?post} = Req, Opts) ->
    BuildId = binary_to_integer(cowboy_req:binding(id, Req)),
    {ok, #{<<"game_id">> := Guid, <<"build_rev">> := BuildRev, <<"is_draft">> := true}} = db_if_game_builds:get_one(BuildId),
    {ok, UploadsDir} = web_config:uploads_dir(),
    Req1 = case cowboy_req:read_part(Req, #{}) of % #{length => MaxFileSize}
        {ok, Headers, R1} ->
            case cow_multipart:form_data(Headers) of
                {data, _FieldName} ->
                    web_util:reply_json_explicit(?http_bad_request, #{error => not_a_file}, R1);
                {file, <<"archive">>, _FilePath, _ContentType} ->
                    % Construct temporary path
                    Prefix = <<Guid/binary, "_", BuildRev/binary>>,
                    TempFilePath = filename:join([UploadsDir, web_util:random_filename(Prefix, <<"zip">>)]),

                    % Track upload process
                    TrackerPid = monitor_upload(self(), TempFilePath),

                    % Unpack archive
                    case stream_file(TempFilePath, R1) of
                        {ok, R2} ->
                            demonitor_upload(TrackerPid),
                            ok = db_if_game_builds:update(BuildId, #{<<"is_processing">> => true}),
                            proc_lib:spawn(fun() -> unpack_archive(TempFilePath, BuildId) end),
                            web_util:reply_json_explicit(?http_ok, #{result => true}, R2);
                        {error, R2} ->
                            web_util:reply_json_explicit(?http_bad_request, #{error => upload_failed}, R2)
                    end;
                {file, _, _, _} ->
                    web_util:reply_json_explicit(?http_bad_request, #{error => invalid_field_name}, R1)
            end;
        {error, Reason} ->
            web_util:reply_json_explicit(?http_internal_server_error, #{error => Reason}, Req)
    end,
    {ok, Req1, Opts};

init(Req, Opts) ->
    Req1 = web_util:reply_json_explicit(?http_bad_request, #{error => invalid_http_method}, Req),
    {ok, Req1, Opts}.

%% Internal functions
monitor_upload(ParentPid, TargetPath) when is_pid(ParentPid) ->
    ?doif(is_process_alive(ParentPid), begin
        proc_lib:spawn(fun() ->
            MonitorRef = erlang:monitor(process, ParentPid),
            monitor_upload_inner(MonitorRef, TargetPath)
        end)
    end);
monitor_upload(_, _) -> undefined.

monitor_upload_inner(MonitorRef, TargetPath) ->
    receive
        {'DOWN', MonitorRef, process, _Pid, _Reason} -> util_file:safe_delete(TargetPath);
        demonitor -> erlang:demonitor(MonitorRef);
        _ -> monitor_upload_inner(MonitorRef, TargetPath)
    end.

demonitor_upload(TrackerPid) when is_pid(TrackerPid) ->
    TrackerPid ! demonitor;
demonitor_upload(_) ->
    ignore.

stream_file(FilePath, Req) ->
    filelib:ensure_dir(FilePath),
    {ok, FileHandle} = file:open(FilePath, [raw, write]),
    try
        {ok, stream_file_inner(FileHandle, Req)}
    catch
        _T:_W:_S -> {error, Req}
    after
        file:close(FileHandle)
    end.

stream_file_inner(FileHandle, Req) ->
    case cowboy_req:read_part_body(Req, #{period => 7200}) of
        {ok, Data, Req1} ->
            file:write(FileHandle, Data),
            Req1;
        {more, Data, Req1} ->
            file:write(FileHandle, Data),
            stream_file_inner(FileHandle, Req1)
    end.

stream_file_to_gzip(BuildId, FilePath, TargetPath, UnzipState) ->
    % Initialize MD5 hash state
    HashState = crypto:hash_init(md5),

    % Initialize ZLib stream to create GZip archive on-the-fly
    Z = zlib:open(),
    MAX_WBITS = 15,
    zlib:deflateInit(Z, default, deflated, 16 + MAX_WBITS, 8, default),

    % Open file for writing
    {ok, FileHandle} = file:open(TargetPath, [raw, write]),

    % Stream file to GZip archive and calculate MD5 on-the-fly
    {ok, #cd_entry{compressed_size = TotalSize, uncompressed_size = UncompressedSize}} = eunzip:entry(UnzipState, FilePath),
    UnpackState = #unpack{build_id = BuildId, file_handle = FileHandle, total_size = TotalSize,  md5 = HashState, z_stream = Z},
    {ok, StreamState} = eunzip:stream_init(UnzipState, FilePath),
    {ok, #unpack{md5 = MD5}} = stream_file_to_gzip_inner(StreamState, UnpackState),

    % Close file
    file:close(FileHandle),

    % Finalize ZLib stream
    zlib:deflateEnd(Z),
    zlib:close(Z),

    % Success
    CompressedSize = filelib:file_size(TargetPath),
    {ok, CompressedSize, UncompressedSize, MD5}.

stream_file_to_gzip_inner(StreamState, #unpack{build_id = BuildId, file_handle = FileHandle, processed_size = ProcessedSize, md5 = HashState, z_stream = Z} = UnpackState) ->
    case eunzip:stream_read_chunk(?chunk_size, StreamState) of
        {ok, Data} ->
            ChunkSize = byte_size(Data),

            % Finalize MD5
            HashState1 = crypto:hash_update(HashState, Data),
            Digest = crypto:hash_final(HashState1),
            MD5 = util_hex:from_binary(Digest),

            % Finalize and close GZip archive
            CompressedData = zlib:deflate(Z, Data, finish),
            file:write(FileHandle, CompressedData),

            % Update counter at database
            ok = db_if_game_builds:inc_processed_size(BuildId, ChunkSize),

            {ok, UnpackState#unpack{processed_size = ProcessedSize + ChunkSize, md5 = MD5}};
        {more, Data, StreamState1} ->
            % Update MD5 state, compress data and write to file
            ChunkSize = byte_size(Data),
            HashState1 = crypto:hash_update(HashState, Data),
            CompressedData = zlib:deflate(Z, Data),
            file:write(FileHandle, CompressedData),

            % Update counter at database
            ok = db_if_game_builds:inc_processed_size(BuildId, ChunkSize),

            stream_file_to_gzip_inner(StreamState1, UnpackState#unpack{processed_size = ProcessedSize + ChunkSize, md5 = HashState1});
        {error, Reason} ->
            {error, Reason}
    end.

common_prefix(FileNames) ->
    PrefixSet = lists:foldl(fun(FileName, AccSet) ->
        case filename:dirname(FileName) of
            <<".">> -> gb_sets:add(undefined, AccSet);
            Dir -> gb_sets:add(Dir, AccSet)
        end
    end, gb_sets:new(), FileNames),
    case gb_sets:is_element(undefined, PrefixSet) of
        true -> <<>>;
        false ->
            ExtendedPrefixSet = lists:foldl(fun(FilePath, AccSet) ->
                FilePathParts = filename:split(FilePath),
                FileSubPaths = lists:map(fun(Count) ->
                    SubParts = lists:sublist(FilePathParts, Count),
                    util_binary:join(SubParts, <<"/">>)
                end, lists:seq(1, length(FilePathParts) - 1)),
                lists:foldl(fun(PathPart, AccSet0) -> gb_sets:add(PathPart, AccSet0) end, AccSet, FileSubPaths)
            end, PrefixSet, gb_sets:to_list(PrefixSet)),
            RegularPrefixes = gb_sets:to_list(PrefixSet),
            ExtendedPrefixes = gb_sets:to_list(ExtendedPrefixSet),
            CommonPrefixes = lists:filter(fun(Dir) ->
                DirLength = byte_size(Dir),
                lists:all(fun
                    (Path) when Path =:= Dir -> true;
                    (<<Path:DirLength/binary, "/", _/binary>>) when Path =:= Dir -> true;
                    (_) -> false
                end, RegularPrefixes)
            end, ExtendedPrefixes),
            SortedCommonPrefixes = lists:sort(fun(A, B) -> byte_size(A) > byte_size(B) end, CommonPrefixes),
            case SortedCommonPrefixes of
                [<<>>|_] -> <<>>;
                [LongestCommonPrefix|_] -> <<LongestCommonPrefix/binary, "/">>;
                _ -> <<>>
            end
    end.

unpack_archive(ArchiveFilePath, BuildId) ->
    try
        % Construct build root directory
        {ok, #{<<"game_id">> := Guid, <<"build_rev">> := BuildRev, <<"is_draft">> := true}} = db_if_game_builds:get_one(BuildId),
        {ok, WwwRoot} = web_config:www_root(),
        BuildRootDirectory = filename:join([WwwRoot, Guid, BuildRev]),
        logger:info("Started unpacking of build archive. Game ID: ~p, build rev. #~s", [Guid, BuildRev]),

        % Open ZIP archive to get file entries and common path prefix
        T1 = time:milliseconds(),
        {ok, UnzipState} = eunzip:open(ArchiveFilePath),
        {ok, Entries} = eunzip:entries(UnzipState),
        FileEntries = [Entry || #cd_entry{is_regular_file = IsRegularFile} = Entry <- Entries, IsRegularFile],
        FileNames = [<<FileName/binary>> || #cd_entry{file_name = FileName} <- FileEntries],
        CommonPrefix = common_prefix(FileNames),

        % Update build at database
        TotalSize = lists:foldl(fun(#cd_entry{uncompressed_size = Size}, Acc) -> Acc + Size end, 0, FileEntries),
        ok = db_if_game_builds:update(BuildId, #{<<"archived_size">> => TotalSize, <<"processed_size">> => 0}),

        % Loop thru files
        [begin
            % Remove common prefix from filename and add .gz extension to construct absolute and relative compressed paths
            RelativePath = ?yesno(CommonPrefix =:= <<>>, FileName, binary:part(FileName, byte_size(CommonPrefix), byte_size(FileName) - byte_size(CommonPrefix))),
            RelativeCompressedPath = <<RelativePath/binary, ".gz">>,
            TargetCompressedPath = filename:join([BuildRootDirectory, RelativeCompressedPath]),
            filelib:ensure_dir(TargetCompressedPath),
            {ok, CompressedSize, UncompressedSize, MD5} = stream_file_to_gzip(BuildId, FileName, TargetCompressedPath, UnzipState),
            {ok, _FileId} = db_if_game_build_files:create(BuildId, RelativePath, UncompressedSize, RelativeCompressedPath, CompressedSize, MD5)
            % logger:info("Streamed file <~s>: ~B bytes", [RelativePath, CompressedSize])
        end || FileName <- FileNames],

        % Success
        T2 = time:milliseconds(),
        logger:info("Successfully unpacked build archive in ~.1f sec. Game ID: ~s, build rev. #~s", [(T2 - T1) / 1000, Guid, BuildRev])
    catch Type:What:StackTrace ->
        % Failure
        FileName = filename:basename(ArchiveFilePath),
        logger:info("Unpacking of build archive ~s failed. Reason: ~p:~p~n", [FileName, Type, What], #{stack_trace => StackTrace})
    after
        db_if_game_builds:update(BuildId, #{<<"is_processing">> => false}),
        util_file:safe_delete(ArchiveFilePath)
    end.

%% Local functions
