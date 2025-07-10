-module(web_build_file_upload).

-behaviour(cowboy_handler).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("http.hrl").
-include("session.hrl").

%% Exported functions

-export([
    init/2
]).

-define(chunk_size, 32 * 1024 * 1024).  % 32 MB

-record(upload, {
    file_size = 0 :: non_neg_integer(),
    md5 :: binary(),
    z_stream :: reference()
}).

%% API

init(#{method := ?post} = Req, Opts) ->
    BuildId = binary_to_integer(cowboy_req:binding(id, Req)),
    {ok, #{<<"game_id">> := Guid, <<"build_rev">> := BuildRev, <<"is_draft">> := true}} = db_if_game_builds:get_one(BuildId),
    {ok, WwwRoot} = web_config:www_root(),
    Req1 = case cowboy_req:read_part(Req, #{}) of % #{length => MaxFileSize}
        {ok, Headers, R1} ->
            case cow_multipart:form_data(Headers) of
                {data, _FieldName} ->
                    web_util:reply_json_explicit(?http_bad_request, #{error => not_a_file}, R1);
                {file, <<"file">>, FilePath, _ContentType} ->
                    % Construct absolute and relative paths both to uncompressed and compressed versions
                    CompressedRelativePath = <<FilePath/binary, ".gz">>,
                    CompressedFullPath = filename:join([WwwRoot, Guid, BuildRev, CompressedRelativePath]),

                    try
                        % Ensure all intermediate directories exist
                        filelib:ensure_dir(CompressedFullPath),

                        % Receive file in a safe manner and calculate compressed file size
                        {FileSize, MD5, R2} = stream_file_to_gzip(CompressedFullPath, R1),
                        CompressedFileSize = filelib:file_size(CompressedFullPath),

                        % Add file entry to database
                        {ok, FileId} = db_if_game_build_files:create(BuildId, FilePath, FileSize, CompressedRelativePath, CompressedFileSize, MD5),
                        web_util:reply_json_explicit(?http_ok, #{file_id => FileId, file_path => FilePath}, R2)
                    catch _:_:_ ->
                        % Delete build file and database entry on error
                        db_if_game_build_files:delete(BuildId, FilePath),
                        util_file:safe_delete(CompressedFullPath),
                        web_util:reply_json_explicit(?http_bad_request, #{error => upload_failed}, R1)
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

%% Local functions

stream_file_to_gzip(FilePath, Req) ->
    % Initialize MD5 hash state
    HashState = crypto:hash_init(md5),

    % Initialize ZLib stream to create GZip archive on-the-fly
    Z = zlib:open(),
    MAX_WBITS = 15,
    zlib:deflateInit(Z, default, deflated, 16 + MAX_WBITS, 8, default),

    % Open file for writing
    {ok, FileHandle} = file:open(FilePath, [raw, write]),

    % Stream file to GZip archive and calculate MD5 on-the-fly
    {FileSize, MD5, Req1} = stream_file_to_gzip_inner(FileHandle, #upload{file_size = 0, md5 = HashState, z_stream = Z}, Req),

    % Close file
    file:close(FileHandle),

    % Finalize ZLib stream
    zlib:deflateEnd(Z),
    zlib:close(Z),

    {FileSize, MD5, Req1}.

stream_file_to_gzip_inner(FileHandle, #upload{file_size = UploadedSize, md5 = HashState, z_stream = Z} = State, Req) ->
    case cowboy_req:read_part_body(Req, #{period => 7200}) of
        {ok, Data, Req1} ->
            ChunkSize = byte_size(Data),

            % Finalize MD5
            HashState1 = crypto:hash_update(HashState, Data),
            Digest = crypto:hash_final(HashState1),
            MD5 = util_hex:from_binary(Digest),

            % Finalize and close GZip archive
            CompressedData = zlib:deflate(Z, Data, finish),
            file:write(FileHandle, CompressedData),
            {UploadedSize + ChunkSize, MD5, Req1};
        {more, Data, Req1} ->
            % Update MD5 state, compress data and write to file
            ChunkSize = byte_size(Data),
            HashState1 = crypto:hash_update(HashState, Data),
            CompressedData = zlib:deflate(Z, Data),
            file:write(FileHandle, CompressedData),
            stream_file_to_gzip_inner(FileHandle, State#upload{file_size = UploadedSize + ChunkSize, md5 = HashState1}, Req1)
    end.
