-module(web_rest_callback_admin_build_file).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("web_protocol.hrl").
-include("session.hrl").
-include("limits.hrl").

%% Exported functions

-export([
    get_build_files/2,
    delete_all_build_files/2,
    delete_build_file/3
]).

%% API

-spec get_build_files(BuildId, Req) -> Response when
    BuildId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:collection(web_protocol:build_file()), cowboy_req:req()}.

get_build_files(BuildId, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, BuildFiles} = db_if_game_build_files:get_all(BuildId),
    BuildFiles1 = lists:map(fun web_protocol:build_file_from_json/1, BuildFiles),
    {#collection{items = BuildFiles1}, Req};

get_build_files(_BuildId, _Req) ->
    % Unauthorized
    web_rest_admin_build_files:get_build_files_403(#forbidden_error{}).

-spec delete_all_build_files(BuildId, Req) -> Response when
    BuildId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:generic_response(), cowboy_req:req()}.

delete_all_build_files(BuildId, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, #{<<"game_id">> := Guid, <<"build_rev">> := BuildRev}} = db_if_game_builds:get_one(BuildId),
    Result = case db_if_game_build_files:delete_all(BuildId) of
        ok -> true;
        _ -> false
    end,
    ?doif(Result, wipe_dir(Guid, BuildRev)),
    {#generic_response{result = Result}, Req};

delete_all_build_files(_BuildId, _Req) ->
    % Unauthorized
    web_rest_admin_build_files:delete_all_build_files_403(#forbidden_error{}).

-spec delete_build_file(BuildId, FileId, Req) -> Response when
    BuildId :: non_neg_integer(),
    FileId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:generic_response(), cowboy_req:req()}.

delete_build_file(BuildId, FileId, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, #{<<"game_id">> := Guid, <<"build_rev">> := BuildRev}} = db_if_game_builds:get_one(BuildId),
    {ok, #{<<"compressed_file_path">> := CompressedFilePath}} = db_if_game_build_files:get_one(FileId),
    Result = case db_if_game_build_files:delete(FileId) of
        ok -> true;
        _ -> false
    end,
    ?doif(Result, wipe_file(Guid, BuildRev, CompressedFilePath)),
    {#generic_response{result = Result}, Req};

delete_build_file(_BuildId, _FileId, _Req) ->
    % Unauthorized
    web_rest_admin_build_file:delete_build_file_403(#forbidden_error{}).

%% Local functions

wipe_file(Guid, BuildRev, FilePath) ->
    {ok, WwwRoot} = web_config:www_root(),
    Path = filename:join([WwwRoot, Guid, BuildRev, FilePath]),
    util_file:safe_delete(Path).

wipe_dir(Guid, BuildRev) ->
    {ok, WwwRoot} = web_config:www_root(),
    Path = filename:join([WwwRoot, Guid, BuildRev]),
    util_file:rm_rf(Path).
