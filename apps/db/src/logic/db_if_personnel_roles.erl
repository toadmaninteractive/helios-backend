-module(db_if_personnel_roles).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/2,
    get_for_account/7,
    get_for_account_count/3,
    get_for_game/6,
    get_for_game_count/2,
    set/3,
    set/5,
    delete/2,
    delete_for_account/1,
    delete_for_game/1
]).

%% API

-spec get_one(PersonnelId, GameId) -> Result when
    PersonnelId :: non_neg_integer(),
    GameId :: binary(),
    Result :: {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one(PersonnelId, GameId) ->
    Query = <<
        "SELECT ",
            "per.id AS personnel_id, ",
            "per.username AS username, ",
            "gm.id AS game_id, ",
            "gm.title AS game_title, ",
            "COALESCE((SELECT jsonb_object_agg(gbr.title, gbr.id) FROM game_branches AS gbr WHERE gbr.game_id = gm.id), '{}'::jsonb) AS game_branches, ",
            "perr.role AS user_role, ",
            "perr.is_global AS is_global, ",
            "perr.branch_ids AS branch_ids, ",
            "COALESCE((",
                "SELECT jsonb_object_agg(t.name, jsonb_build_object('role', t.role, 'is_global', t.is_global, 'branch_ids', t.branch_ids)) ",
                "FROM ( ",
                    "SELECT pg.name, pgr.role, pgr.is_global, pgr.branch_ids ",
                    "FROM personnel_group_roles AS pgr ",
                    "LEFT OUTER JOIN personnel_groups AS pg ON (pg.id = pgr.group_id) ",
                    "LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.personnel_id = $1) ",
                    "WHERE pgr.game_id = gm.id AND pgr.group_id = pergm.group_id ",
                    "GROUP BY pg.name, pgr.role, pgr.is_global, pgr.branch_ids ",
                ") AS t ",
            "), '{}'::jsonb) AS group_roles ",
        "FROM games AS gm ",
        "LEFT OUTER JOIN personnel AS per ON (per.id = $1) ",
        "LEFT OUTER JOIN personnel_roles AS perr ON (perr.game_id = gm.id AND perr.personnel_id = $1) ",
        "WHERE gm.id = $2"
    >>,
    case db_query:select_one(Query, [PersonnelId, GameId]) of
        {ok, AccountRole} -> {ok, AccountRole};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_for_account(PersonnelId, Needle, OrderBy, OrderDir, Offset, Limit, ActiveOnly) -> Result when
    PersonnelId :: binary(),
    Needle :: binary() | 'undefined',
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    ActiveOnly :: boolean(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_for_account(PersonnelId, Needle, OrderBy, OrderDir, Offset, Limit, ActiveOnly) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<" AND (strpos(LOWER(gm.id), LOWER($2)) > 0 OR strpos(LOWER(gm.title), LOWER($2)) > 0)">>, <<>>),
    ActivePart = ?yesno(ActiveOnly, <<" AND is_game_accessible_by_personnel(gm.id, $1) ">>, <<>>),
    AdditionalOrder = ?yesno(OrderBy =:= <<"user_role">>, [{game_id, asc}], []),
    OrderList = [{OrderBy, OrderDir, false}] ++ AdditionalOrder,
    Filter = db_util:mk_query_filter_ex(OrderList, Offset, Limit),
    Query = <<
        "SELECT ",
            "per.id AS personnel_id, ",
            "per.username AS username, ",
            "gm.id AS game_id, ",
            "gm.title AS game_title, ",
            "COALESCE((SELECT jsonb_object_agg(gbr.title, gbr.id) FROM game_branches AS gbr WHERE gbr.game_id = gm.id), '{}'::jsonb) AS game_branches, ",
            "perr.role AS user_role, ",
            "perr.is_global AS is_global, ",
            "perr.branch_ids AS branch_ids, ",
            "COALESCE((",
                "SELECT jsonb_object_agg(t.name, jsonb_build_object('role', t.role, 'is_global', t.is_global, 'branch_ids', t.branch_ids)) ",
                "FROM ( ",
                    "SELECT pg.name, pgr.role, pgr.is_global, pgr.branch_ids ",
                    "FROM personnel_group_roles AS pgr ",
                    "LEFT OUTER JOIN personnel_groups AS pg ON (pg.id = pgr.group_id) ",
                    "LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.personnel_id = $1) ",
                    "WHERE pgr.game_id = gm.id AND pgr.group_id = pergm.group_id ",
                    "GROUP BY pg.name, pgr.role, pgr.is_global, pgr.branch_ids ",
                ") AS t ",
            "), '{}'::jsonb) AS group_roles ",
        "FROM games AS gm ",
        "LEFT OUTER JOIN personnel AS per ON (per.id = $1) ",
        "LEFT OUTER JOIN personnel_roles AS perr ON (perr.game_id = gm.id AND perr.personnel_id = $1) ",
        "WHERE TRUE ",
        NeedlePart/binary,
        ActivePart/binary,
        Filter/binary
    >>,
    Params = [PersonnelId] ++ ?yesno(HasNeedle, [Needle], []),
    db_query:select(Query, Params).

-spec get_for_account_count(PersonnelId, Needle, ActiveOnly) -> Result when
    PersonnelId :: binary(),
    Needle :: binary() | 'undefined',
    ActiveOnly :: boolean(),
    Result :: {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_for_account_count(PersonnelId, Needle, ActiveOnly) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<" AND (strpos(LOWER(gm.id), LOWER($2)) > 0 OR strpos(LOWER(gm.title), LOWER($2)) > 0)">>, <<>>),
    ActivePart = ?yesno(ActiveOnly, <<" AND is_game_accessible_by_personnel(gm.id, $1) ">>, <<>>),
    Query = <<
        "SELECT COUNT(*)::bigint AS count ",
        "FROM games AS gm ",
        "LEFT OUTER JOIN personnel_roles AS perr ON (perr.game_id = gm.id AND perr.personnel_id = $1) ",
        "WHERE TRUE ",
        NeedlePart/binary,
        ActivePart/binary
    >>,
    Params = [PersonnelId] ++ ?yesno(HasNeedle, [Needle], []),
    case db_query:select_one(Query, Params) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_for_game(GameId, Needle, OrderBy, OrderDir, Offset, Limit) -> Result when
    GameId :: binary(),
    Needle :: binary() | 'undefined',
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_for_game(GameId, Needle, OrderBy, OrderDir, Offset, Limit) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<
        " AND (",
            "strpos(LOWER(q.username), LOWER($2)) > 0 ",
            "OR (",
                "SELECT COUNT(joke.*)::bigint > 0 ",
                "FROM jsonb_object_keys(q.group_roles) AS joke ",
                "WHERE strpos(LOWER(joke), LOWER($2)) > 0",
            ")",
        ") "
    >>, <<>>),
    AdditionalOrder = ?yesno(OrderBy =:= <<"user_role">>, [{username, asc}], []),
    OrderList = [{OrderBy, OrderDir, false}] ++ AdditionalOrder,
    Filter = db_util:mk_query_filter_ex(OrderList, Offset, Limit),
    Query = <<
        "SELECT ",
            "q.personnel_id AS personnel_id, ",
            "q.username AS username, ",
            "q.game_id AS game_id, ",
            "q.game_title AS game_title, ",
            "q.game_branches AS game_branches, ",
            "q.user_role AS user_role, ",
            "q.is_global AS is_global, ",
            "q.branch_ids AS branch_ids, ",
            "q.group_roles AS group_roles ",
        "FROM (",
            "SELECT ",
                "per.id AS personnel_id, ",
                "per.username AS username, ",
                "gm.id AS game_id, ",
                "gm.title AS game_title, ",
                "COALESCE((SELECT jsonb_object_agg(gbr.title, gbr.id) FROM game_branches AS gbr WHERE gbr.game_id = gm.id), '{}'::jsonb) AS game_branches, ",
                "perr.role AS user_role, ",
                "perr.is_global AS is_global, ",
                "perr.branch_ids AS branch_ids, ",
                "COALESCE((",
                    "SELECT jsonb_object_agg(t.name, jsonb_build_object('role', t.role, 'is_global', t.is_global, 'branch_ids', t.branch_ids)) ",
                    "FROM ( ",
                        "SELECT pg.name, pgr.role, pgr.is_global, pgr.branch_ids ",
                        "FROM personnel_group_roles AS pgr ",
                        "LEFT OUTER JOIN personnel_groups AS pg ON (pg.id = pgr.group_id) ",
                        "LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.personnel_id = per.id) ",
                        "WHERE pgr.game_id = $1 AND pgr.group_id = pergm.group_id ",
                        "GROUP BY pg.name, pgr.role, pgr.is_global, pgr.branch_ids ",
                    ") AS t ",
                "), '{}'::jsonb) AS group_roles ",
            "FROM personnel AS per ",
            "LEFT OUTER JOIN games AS gm ON (gm.id = $1) ",
            "LEFT OUTER JOIN personnel_roles AS perr ON (perr.personnel_id = per.id AND perr.game_id = $1) ",
            "WHERE NOT per.is_blocked AND NOT per.is_deleted ",
        ") AS q ",
        "WHERE TRUE ",
        NeedlePart/binary,
        Filter/binary
    >>,
    Params = [GameId] ++ ?yesno(HasNeedle, [Needle], []),
    db_query:select(Query, Params).

-spec get_for_game_count(GameId, Needle) -> Result when
    GameId :: binary(),
    Needle :: binary() | 'undefined',
    Result :: {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_for_game_count(GameId, Needle) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<
        " AND (",
            "strpos(LOWER(q.username), LOWER($2)) > 0 ",
            "OR (",
                "SELECT COUNT(joke.*)::bigint > 0 ",
                "FROM jsonb_object_keys(q.group_roles) AS joke ",
                "WHERE strpos(LOWER(joke), LOWER($2)) > 0",
            ")",
        ") "
    >>, <<>>),
    Query = <<
        "SELECT COUNT(q.*)::bigint AS count ",
        "FROM (",
            "SELECT ",
                "per.id AS personnel_id, ",
                "per.username AS username, ",
                "gm.id AS game_id, ",
                "gm.title AS game_title, ",
                "COALESCE((SELECT jsonb_object_agg(gbr.title, gbr.id) FROM game_branches AS gbr WHERE gbr.game_id = gm.id), '{}'::jsonb) AS game_branches, ",
                "perr.role AS user_role, ",
                "perr.is_global AS is_global, ",
                "perr.branch_ids AS branch_ids, ",
                "COALESCE((",
                    "SELECT jsonb_object_agg(t.name, jsonb_build_object('role', t.role, 'is_global', t.is_global, 'branch_ids', t.branch_ids)) ",
                    "FROM ( ",
                        "SELECT pg.name, pgr.role, pgr.is_global, pgr.branch_ids ",
                        "FROM personnel_group_roles AS pgr ",
                        "LEFT OUTER JOIN personnel_groups AS pg ON (pg.id = pgr.group_id) ",
                        "LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.personnel_id = per.id) ",
                        "WHERE pgr.game_id = $1 AND pgr.group_id = pergm.group_id ",
                        "GROUP BY pg.name, pgr.role, pgr.is_global, pgr.branch_ids ",
                    ") AS t ",
                "), '{}'::jsonb) AS group_roles ",
            "FROM personnel AS per ",
            "LEFT OUTER JOIN games AS gm ON (gm.id = $1) ",
            "LEFT OUTER JOIN personnel_roles AS perr ON (perr.personnel_id = per.id AND perr.game_id = $1) ",
            "WHERE NOT per.is_blocked AND NOT per.is_deleted ",
        ") AS q ",
        "WHERE TRUE ",
        NeedlePart/binary
    >>,
    Params = [GameId] ++ ?yesno(HasNeedle, [Needle], []),
    case db_query:select_one(Query, Params) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec set(PersonnelId, GameId, Role) -> Result when
    PersonnelId :: non_neg_integer(),
    GameId :: binary(),
    Role :: atom(),
    Result :: 'ok' |  {'error', Reason :: atom()}.

set(PersonnelId, GameId, Role) ->
    Query = <<
        "INSERT INTO personnel_roles (personnel_id, game_id, role) ",
        "VALUES ($1, $2, $3) ",
        "ON CONFLICT (\"personnel_id\", \"game_id\") DO UPDATE SET role = EXCLUDED.role "
    >>,
    Params = [PersonnelId, GameId, Role],
    case db_query:insert(Query, Params) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec set(PersonnelId, GameId, Role, IsGlobal, BranchIds) -> Result when
    PersonnelId :: non_neg_integer(),
    GameId :: binary(),
    Role :: atom(),
    IsGlobal :: boolean(),
    BranchIds :: [non_neg_integer()],
    Result :: 'ok' |  {'error', Reason :: atom()}.

set(PersonnelId, GameId, Role, IsGlobal, BranchIds) ->
    Query = <<
        "INSERT INTO personnel_roles (personnel_id, game_id, role, is_global, branch_ids) ",
        "VALUES ($1, $2, $3, $4, $5) ",
        "ON CONFLICT (\"personnel_id\", \"game_id\") DO UPDATE ",
        "SET role = EXCLUDED.role, is_global = EXCLUDED.is_global, branch_ids = EXCLUDED.branch_ids "
    >>,
    Params = [PersonnelId, GameId, Role, IsGlobal, jsx:encode(BranchIds)],
    case db_query:insert(Query, Params) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec delete(PersonnelId, GameId) -> Result when
    PersonnelId :: non_neg_integer(),
    GameId :: binary(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete(PersonnelId, GameId) ->
    Query = <<"DELETE FROM personnel_roles WHERE personnel_id = $1 AND game_id = $2">>,
    case db_query:delete(Query, [PersonnelId, GameId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec delete_for_account(PersonnelId) -> Result when
    PersonnelId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete_for_account(PersonnelId) ->
    Query = <<"DELETE FROM personnel_roles WHERE personnel_id = $1">>,
    case db_query:delete(Query, [PersonnelId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec delete_for_game(GameId) -> Result when
    GameId :: binary(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete_for_game(GameId) ->
    Query = <<"DELETE FROM personnel_roles WHERE game_id = $1">>,
    case db_query:delete(Query, [GameId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

%% Local functions
