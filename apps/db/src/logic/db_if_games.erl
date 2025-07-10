-module(db_if_games).

%% Include files

-include_lib("epgsql/include/epgsql.hrl").
-include_lib("aplib/include/apmacros.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get/6,
    get_count/2,
    get_all/0,
    get_all_for_platform/1,
    get_accessible/1,
    get_accessible_for_platform/2,
    get_favourites/2,
    create/4,
    update/2,
    update/3,
    delete/1,
    undelete/1,
    publish/1,
    unpublish/1,
    enable/1,
    disable/1,
    exists/1,
    set_jira_key/2,
    set_selene_key/2,
    set_discord_url/2,
    is_accessible/2,
    is_favourite/2,
    set_favourite/2,
    unset_favourite/2
]).

%% API

-spec get_one(Guid :: binary()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one(Guid) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM games AS gm ",
        (common_joins())/binary,
        "WHERE gm.id = TRIM($1)"
    >>,
    case db_query:select_one(Query, [Guid]) of
        {ok, Item} -> {ok, Item};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get(PersonnelId, Needle, OrderBy, OrderDir, Offset, Limit) -> Result when
    PersonnelId :: non_neg_integer(),
    Needle :: binary() | 'undefined',
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(), Limit :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get(PersonnelId, Needle, OrderBy, OrderDir, Offset, Limit) ->
    Role = uploader,
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<"AND (strpos(LOWER(gm.id), LOWER($3)) > 0 OR strpos(LOWER(gm.title), LOWER($3)) > 0)">>, <<>>),
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM games AS gm ",
        (common_joins())/binary,
        "WHERE is_game_access_role_sufficient(gm.id, $1, $2) ",
        NeedlePart/binary,
        Filter/binary
    >>,
    Params = [PersonnelId, Role] ++ ?yesno(HasNeedle, [Needle], []),
    db_query:select(Query, Params).

-spec get_count(PersonnelId, Needle) -> Result when
    PersonnelId :: non_neg_integer(),
    Needle :: binary() | 'undefined',
    Result :: {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_count(PersonnelId, Needle) ->
    Role = uploader,
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<"AND (strpos(LOWER(gm.id), LOWER($3)) > 0 OR strpos(LOWER(gm.title), LOWER($3)) > 0)">>, <<>>),
    Query = <<
        "SELECT COUNT(*)::bigint AS count ",
        "FROM games AS gm ",
        "WHERE is_game_access_role_sufficient(gm.id, $1, $2) ",
        NeedlePart/binary
    >>,
    Params = [PersonnelId, Role] ++ ?yesno(HasNeedle, [Needle], []),
    case db_query:select_one(Query, Params) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_all() ->
    {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_all() ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM games AS gm ",
        (common_joins())/binary,
        "ORDER BY title ASC"
    >>,
    db_query:select(Query, []).

-spec get_all_for_platform(Platform :: web_protocol:platform()) ->
    {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_all_for_platform(Platform) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM games AS gm ",
        (common_joins())/binary,
        "LEFT OUTER JOIN game_branches AS gbr ON (gbr.game_id = gm.id AND gbr.platform = $1) ",
        "WHERE gbr.id IS NOT NULL ",
        "ORDER BY title ASC"
    >>,
    db_query:select(Query, [web_protocol:platform_to_json(Platform)]).

-spec get_accessible(PersonnelId :: non_neg_integer()) ->
    {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_accessible(PersonnelId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM games AS gm ",
        (common_joins())/binary,
        "WHERE is_game_accessible_by_personnel(gm.id, $1) ",
        "ORDER BY title ASC"
    >>,
    db_query:select(Query, [PersonnelId]).

-spec get_accessible_for_platform(PersonnelId, Platform) -> Result when
    PersonnelId :: non_neg_integer(),
    Platform :: web_protocol:platform(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_accessible_for_platform(PersonnelId, Platform) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM games AS gm ",
        (common_joins())/binary,
        "LEFT OUTER JOIN game_branches AS gbr ON (gbr.game_id = gm.id AND gbr.platform = $2) ",
        "WHERE gbr.id IS NOT NULL AND is_game_accessible_by_personnel(gm.id, $1) ",
        "GROUP BY gm.id ",
        "ORDER BY gm.title ASC "
    >>,
    Params = [PersonnelId, web_protocol:platform_to_json(Platform)],
    db_query:select(Query, Params).

-spec get_favourites(PersonnelId, MinRole) -> Result when
    PersonnelId :: non_neg_integer(),
    MinRole :: web_protocol:access_role(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_favourites(PersonnelId, MinRole) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM games AS gm ",
        (common_joins())/binary,
        "LEFT OUTER JOIN game_branches AS gbr ON (gbr.game_id = gm.id) ",
        "LEFT OUTER JOIN personnel_favourite_games AS perfg ON (perfg.game_id = gm.id AND perfg.personnel_id = $1) ",
        "WHERE gbr.id IS NOT NULL AND is_game_access_role_sufficient(gm.id, $1, $2) AND perfg.game_id IS NOT NULL ",
        "GROUP BY gm.id ",
        "ORDER BY gm.title ASC "
    >>,
    db_query:select(Query, [PersonnelId, MinRole]).

-spec create(Guid :: binary(), Title :: binary(), Price :: non_neg_integer(), Currency :: binary()) ->
    {'ok', Guid :: binary()} | {'error', Reason :: atom()}.

create(Guid, Title, Price, Currency) ->
    Query = <<
        "INSERT INTO games (id, title, price, currency) ",
        "VALUES (trim($1), trim($2), $3, trim($4)) ",
        "RETURNING id::varchar"
    >>,
    Params = [Guid, Title, Price, Currency],
    case db_query:insert(Query, Params, [raw_error]) of
        {ok, 1, Columns, Rows} ->
            [#{<<"id">> := GameId}] = db_util:result_to_json(Columns, Rows),
            {ok, GameId};
        {error, #error{codename = unique_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"games_pkey">> -> {error, game_id_already_exists};
                <<"game_title_ult_index">> -> {error, game_title_already_exists}
            end;
        {error, #error{codename = Code}} ->
            {error, Code}
    end.

-spec update(Guid :: binary(), Patch :: maps:map()) ->
    'ok' | {'error', Reason :: atom()}.

update(Guid, Patch) ->
    update(Guid, undefined, Patch).

-spec update(Guid :: binary(), Rev :: non_neg_integer() | 'undefined', Patch :: maps:map()) ->
    'ok' | {'error', Reason :: atom()}.

update(Guid, Rev, Patch) ->
    % Define all possible fields
    Fields = [
        ?mk_mod_trim(title),
        ?mk_mod_trim(description),
        ?mk_mod_trim(jira_key),
        ?mk_mod_trim(selene_key),
        ?mk_mod_trim(ci_url),
        ?mk_mod_trim(discord_url),
        price,
        ?mk_mod_trim(currency),
        build_lifetime,
        is_published,
        is_disabled,
        is_deleted
    ],

    % Define auto-set fields
    AutoSetFields = [
        ?mk_mod_inc(rev),
        ?mk_mod_set_now(updated_at)
    ],

    % Perform update
    case db_util:mk_update(<<"games">>, <<"id">>, Guid, <<"rev">>, Rev, Fields, AutoSetFields, Patch) of
        {ok, Query, Params} ->
            case db_query:update(Query, Params, [raw_error]) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, not_exists};
                {error, #error{codename = unique_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"game_title_ult_index">> -> {error, game_title_already_exists}
                    end;
                {error, #error{codename = check_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"games_title_check">> -> {error, invalid_game_title};
                        <<"build_lifetime_check">> -> {error, build_lifetime}
                    end;
                {error, #error{codename = Code}} -> {error, Code}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete(Guid :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

delete(Guid) ->
    update(Guid, #{<<"is_deleted">> => true}).

-spec undelete(Guid :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

undelete(Guid) ->
    update(Guid, #{<<"is_deleted">> => false}).

-spec publish(Guid :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

publish(Guid) ->
    update(Guid, #{<<"is_published">> => true}).

-spec unpublish(Guid :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

unpublish(Guid) ->
    update(Guid, #{<<"is_published">> => false}).

-spec enable(Guid :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

enable(Guid) ->
    update(Guid, #{<<"is_disabled">> => false}).

-spec disable(Guid :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

disable(Guid) ->
    update(Guid, #{<<"is_disabled">> => true}).

-spec exists(Guid :: binary()) ->
    {'ok', boolean()} | {'error', Reason :: atom()}.

exists(Guid) ->
    Query = <<"SELECT COUNT(*)::bigint AS count FROM games WHERE id = TRIM($1)">>,
    case db_query:select_one(Query, [Guid]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count > 0};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec set_jira_key(Guid :: binary(), JiraKey :: binary() | 'null') ->
    'ok' | {'error', Reason :: atom()}.

set_jira_key(Guid, JiraKey) ->
    update(Guid, #{<<"jira_key">> => JiraKey}).

-spec set_selene_key(Guid :: binary(), SeleneKey :: binary() | 'null') ->
    'ok' | {'error', Reason :: atom()}.

set_selene_key(Guid, SeleneKey) ->
    update(Guid, #{<<"selene_key">> => SeleneKey}).

-spec set_discord_url(Guid :: binary(), DiscordUrl :: binary() | 'null') ->
    'ok' | {'error', Reason :: atom()}.

set_discord_url(Guid, DiscordUrl) ->
    update(Guid, #{<<"discord_url">> => DiscordUrl}).

-spec is_accessible(Guid, PersonnelId) -> Result when
    Guid :: binary(),
    PersonnelId :: non_neg_integer(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

is_accessible(Guid, PersonnelId) ->
    Query = <<"SELECT is_game_accessible_by_personnel($1, $2) AS result">>,
    case db_query:select_one(Query, [Guid, PersonnelId]) of
        {ok, #{<<"result">> := Result}} -> {ok, Result};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec is_favourite(Guid, PersonnelId) -> Result when
    Guid :: binary(),
    PersonnelId :: non_neg_integer(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

is_favourite(Guid, PersonnelId) ->
    Query = <<
        "SELECT COUNT(*)::bigint > 0 AS result ",
        "FROM personnel_favourite_games ",
        "WHERE game_id = TRIM($1) AND personnel_id = $2"
    >>,
    case db_query:select_one(Query, [Guid, PersonnelId]) of
        {ok, #{<<"result">> := Result}} -> {ok, Result};
        {error, Reason} -> {error, Reason}
    end.

-spec set_favourite(Guid, PersonnelId) -> Result when
    Guid :: binary(),
    PersonnelId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

set_favourite(Guid, PersonnelId) ->
    Query = <<
        "INSERT INTO personnel_favourite_games (personnel_id, game_id) ",
        "VALUES ($1, TRIM($2)) ",
        "ON CONFLICT DO NOTHING "
    >>,
    Params = [PersonnelId, Guid],
    case db_query:insert(Query, Params) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec unset_favourite(Guid, PersonnelId) -> Result when
    Guid :: binary(),
    PersonnelId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

unset_favourite(Guid, PersonnelId) ->
    Query = <<"DELETE FROM personnel_favourite_games WHERE game_id = TRIM($1) AND personnel_id = $2">>,
    case db_query:delete(Query, [Guid, PersonnelId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % gm : games
    <<
        "gm.id AS id, ",
        "gm.rev AS rev, ",
        "gm.title AS title, ",
        "gm.description AS description, ",
        "gm.jira_key AS jira_key, ",
        "gm.selene_key AS selene_key, ",
        "gm.ci_url AS ci_url, ",
        "gm.discord_url AS discord_url, ",
        "gm.price AS price, ",
        "gm.currency AS currency, ",
        "gm.build_lifetime AS build_lifetime, ",
        "assigned_game_categories(gm.id) AS categories,"
        "gm.is_published AS is_published, ",
        "gm.is_disabled AS is_disabled, ",
        "gm.is_deleted AS is_deleted, ",
        "gm.created_at AS created_at, ",
        "gm.updated_at AS updated_at "
    >>.

common_joins() ->
    <<>>.
