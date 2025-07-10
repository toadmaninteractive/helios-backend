-module(web_helper).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("web_protocol.hrl").

%% Exported functions

-export([
    to_game_item/1,
    to_game_branch_item/1,
    is_branch_available/2
]).

%% API

-spec to_game_item(jsx:json_term()) -> web_protocol:game_item().

to_game_item(#{
    <<"id">> := Guid,
    <<"title">> := Title,
    <<"description">> := Desc,
    <<"categories">> := Categories,
    <<"price">> := Price,
    <<"currency">> := Currency,
    <<"jira_key">> := JiraKey,
    <<"selene_key">> := SeleneKey,
    <<"discord_url">> := DiscordUrl
}) ->
    #game_item{
        guid = Guid,
        title = Title,
        description = Desc,
        categories = Categories,
        price = #price{
            amount = round(Price),
            currency = currency_to_igor(Currency)
        },
        jira_key = web_util:maybe_null(JiraKey),
        selene_key = web_util:maybe_null(SeleneKey),
        discord_url = web_util:maybe_null(DiscordUrl)
    }.

-spec to_game_branch_item(jsx:json_term()) -> web_protocol:game_branch_item().

to_game_branch_item(#{
    <<"title">> := Name,
    <<"game_engine">> := GameEngine,
    <<"is_reportable">> := IsReportable,
    <<"is_default">> := IsDefault,
    <<"build_rev">> := Build,
    <<"build_change_list">> := BuildChangeList,
    <<"build_created_at">> := BuildTime,
    <<"build_total_size">> := Size,
    <<"build_compressed_size">> := CompressedSize,
    <<"build_exe_path">> := ExePath,
    <<"build_log_path">> := LogPath,
    <<"build_crash_report_path">> := CrashReportPath,
    <<"build_config_path">> := ConfigPath,
    <<"build_optional_file_masks">> := OptionalFileMasks,
    <<"build_preserved_file_masks">> := PreservedFileMasks,
    <<"build_redistributables">> := Redistributables,
    <<"build_pdb_files">> := PdbFiles,
    <<"build_cdn_root_url">> := RootUrl,
    <<"ini_config">> := IniConfigEntries,
    <<"registry_config">> := RegistryConfigEntries
}) ->
    #game_branch_item{
        name = Name,
        is_default = IsDefault,
        build = Build,
        build_time = iso8601:parse_datetimems(BuildTime),
        build_change_list = web_util:maybe_null(BuildChangeList),
        size = Size,
        compressed_size = CompressedSize,
        exe_path = ExePath,
        log_path = LogPath,
        crash_report_path = CrashReportPath,
        config_path = ConfigPath,
        optional_file_masks = OptionalFileMasks,
        preserved_file_masks = PreservedFileMasks,
        redistributables = [web_protocol:redistributable_entry_from_json(R) || R <- Redistributables],
        pdb_files = PdbFiles,
        root_url = RootUrl,
        game_engine = maybe_game_engine(GameEngine),
        is_reportable = IsReportable,
        ini_config = [web_protocol:ini_file_entry_from_json(ICE) || ICE <- IniConfigEntries],
        registry_config = [web_protocol:registry_config_entry_from_json(Reg) || Reg <- RegistryConfigEntries]
    }.

-spec is_branch_available(BranchId :: non_neg_integer(), AccessData :: jsx:json_term()) -> boolean().

is_branch_available(BranchId, AccessData) ->
    #{<<"user_role">> := UserRole, <<"is_global">> := IsGlobal, <<"branch_ids">> := BranchIds, <<"group_roles">> := GroupRoles} = AccessData,
    case is_available(BranchId, UserRole, IsGlobal, BranchIds) of
        true -> true;
        false -> check_groups(BranchId, maps:values(GroupRoles))
    end.

%% Local functions

currency_to_igor(Currency) ->
    binary_to_atom(util_binary:to_lower(Currency), latin1).

maybe_game_engine(null) -> undefined;
maybe_game_engine(GameEngine) -> web_protocol:game_engine_from_json(GameEngine).

is_available(BranchId, Role, IsGlobal, BranchIds) ->
    ActualRole = ?yesno(Role =:= null, undefined, web_protocol:access_role_from_json(Role)),
    ActualIsGlobal = ?yesno(is_boolean(IsGlobal), IsGlobal, false),
    ActualBranchIds = ?yesno(is_list(BranchIds), BranchIds, []),
    case ActualRole of
        undefined -> false;
        consumer -> ActualIsGlobal orelse lists:member(BranchId, ActualBranchIds);
        _ -> true
    end.

check_groups(_BranchId, []) -> false;
check_groups(BranchId, [#{<<"role">> := Role, <<"is_global">> := IsGlobal, <<"branch_ids">> := BranchIds} | Rest]) ->
    HasAccess = is_available(BranchId, Role, IsGlobal, BranchIds),
    ?yesno(HasAccess, HasAccess, check_groups(BranchId, Rest)).
