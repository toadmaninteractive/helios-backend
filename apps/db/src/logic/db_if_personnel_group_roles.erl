-module(db_if_personnel_group_roles).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/2,
    get_for_group/6,
    get_for_group_count/2,
    get_for_game/6,
    get_for_game_count/2,
    set/3,
    set/5,
    delete/2,
    delete_for_group/1,
    delete_for_game/1
]).

%% API

-spec get_one(GroupId, GameId) -> Result when
    GroupId :: non_neg_integer(),
    GameId :: binary(),
    Result :: {'ok', binary()} | {'error', Reason :: atom()}.

get_one(GroupId, GameId) ->
    Query = <<"SELECT role FROM personnel_group_roles WHERE group_id = $1 AND game_id = $2">>,
    case db_query:select_one(Query, [GroupId, GameId]) of
        {ok, #{<<"role">> := Role}} -> {ok, Role};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_for_group(GroupId, Needle, OrderBy, OrderDir, Offset, Limit) -> Result when
    GroupId :: binary(),
    Needle :: binary() | 'undefined',
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_for_group(GroupId, Needle, OrderBy, OrderDir, Offset, Limit) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<" WHERE (strpos(LOWER(gm.id), LOWER($2)) > 0 OR strpos(LOWER(gm.title), LOWER($2)) > 0) ">>, <<>>),
    AdditionalOrder = ?yesno(OrderBy =:= <<"group_role">>, [{game_id, asc}], []),
    OrderList = [{OrderBy, OrderDir, false}] ++ AdditionalOrder,
    Filter = db_util:mk_query_filter_ex(OrderList, Offset, Limit),
    Query = <<
        "SELECT ",
            "perg.id AS group_id, ",
            "perg.name AS group_name, ",
            "gm.id AS game_id, ",
            "gm.title AS game_title, ",
            "COALESCE((SELECT jsonb_object_agg(gbr.title, gbr.id) FROM game_branches AS gbr WHERE gbr.game_id = gm.id), '{}'::jsonb) AS game_branches, ",
            "pergr.role AS group_role, ",
            "pergr.is_global AS is_global, ",
            "pergr.branch_ids AS branch_ids ",
        "FROM games AS gm ",
        "LEFT OUTER JOIN personnel_groups AS perg ON (perg.id = $1) ",
        "LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.game_id = gm.id AND pergr.group_id = $1) ",
        NeedlePart/binary,
        Filter/binary
    >>,
    Params = [GroupId] ++ ?yesno(HasNeedle, [Needle], []),
    db_query:select(Query, Params).

-spec get_for_group_count(GroupId, Needle) -> Result when
    GroupId :: binary(),
    Needle :: binary() | 'undefined',
    Result :: {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_for_group_count(GroupId, Needle) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<" WHERE (strpos(LOWER(gm.id), LOWER($2)) > 0 OR strpos(LOWER(gm.title), LOWER($2)) > 0)">>, <<>>),
    Query = <<
        "SELECT COUNT(*)::bigint AS count ",
        "FROM games AS gm ",
        "LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.game_id = gm.id AND pergr.group_id = $1) ",
        NeedlePart/binary
    >>,
    Params = [GroupId] ++ ?yesno(HasNeedle, [Needle], []),
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
    NeedlePart = ?yesno(HasNeedle, <<" AND strpos(LOWER(perg.name), LOWER($2)) > 0 ">>, <<>>),
    AdditionalOrder = ?yesno(OrderBy =:= <<"group_role">>, [{group_name, asc}], []),
    OrderList = [{OrderBy, OrderDir, false}] ++ AdditionalOrder,
    Filter = db_util:mk_query_filter_ex(OrderList, Offset, Limit),
    Query = <<
        "SELECT ",
            "perg.id AS group_id, ",
            "perg.name AS group_name, ",
            "gm.id AS game_id, ",
            "gm.title AS game_title, ",
            "COALESCE((SELECT jsonb_object_agg(gbr.title, gbr.id) FROM game_branches AS gbr WHERE gbr.game_id = gm.id), '{}'::jsonb) AS game_branches, ",
            "pergr.role AS group_role, ",
            "pergr.is_global AS is_global, ",
            "pergr.branch_ids AS branch_ids ",
        "FROM personnel_groups AS perg ",
        "LEFT OUTER JOIN games AS gm ON (gm.id = $1) ",
        "LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = perg.id AND pergr.game_id = $1) ",
        "WHERE NOT perg.is_deleted ",
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
    NeedlePart = ?yesno(HasNeedle, <<" AND strpos(LOWER(perg.name), LOWER($2)) > 0">>, <<>>),
    Query = <<
        "SELECT COUNT(*)::bigint AS count ",
        "FROM personnel_groups AS perg ",
        "LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = perg.id AND pergr.game_id = $1) ",
        "WHERE NOT perg.is_deleted ",
        NeedlePart/binary
    >>,
    Params = [GameId] ++ ?yesno(HasNeedle, [Needle], []),
    case db_query:select_one(Query, Params) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec set(GroupId, GameId, Role) -> Result when
    GroupId :: non_neg_integer(),
    GameId :: binary(),
    Role :: atom(),
    Result :: 'ok' |  {'error', Reason :: atom()}.

set(GroupId, GameId, Role) ->
    Query = <<
        "INSERT INTO personnel_group_roles (group_id, game_id, role) ",
        "VALUES ($1, $2, $3) ",
        "ON CONFLICT (\"group_id\", \"game_id\") DO UPDATE SET role = EXCLUDED.role "
    >>,
    Params = [GroupId, GameId, Role],
    case db_query:insert(Query, Params) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec set(GroupId, GameId, Role, IsGlobal, BranchIds) -> Result when
    GroupId :: non_neg_integer(),
    GameId :: binary(),
    Role :: atom(),
    IsGlobal :: boolean(),
    BranchIds :: [non_neg_integer()],
    Result :: 'ok' |  {'error', Reason :: atom()}.

set(GroupId, GameId, Role, IsGlobal, BranchIds) ->
    Query = <<
        "INSERT INTO personnel_group_roles (group_id, game_id, role, is_global, branch_ids) ",
        "VALUES ($1, $2, $3, $4, $5) ",
        "ON CONFLICT (\"group_id\", \"game_id\") DO UPDATE ",
        "SET role = EXCLUDED.role, is_global = EXCLUDED.is_global, branch_ids = EXCLUDED.branch_ids "
    >>,
    Params = [GroupId, GameId, Role, IsGlobal, jsx:encode(BranchIds)],
    case db_query:insert(Query, Params) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec delete(GroupId, GameId) -> Result when
    GroupId :: non_neg_integer(),
    GameId :: binary(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete(GroupId, GameId) ->
    Query = <<"DELETE FROM personnel_group_roles WHERE group_id = $1 AND game_id = $2">>,
    case db_query:delete(Query, [GroupId, GameId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec delete_for_group(GroupId) -> Result when
    GroupId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete_for_group(GroupId) ->
    Query = <<"DELETE FROM personnel_group_roles WHERE group_id = $1">>,
    case db_query:delete(Query, [GroupId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec delete_for_game(GameId) -> Result when
    GameId :: binary(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete_for_game(GameId) ->
    Query = <<"DELETE FROM personnel_group_roles WHERE game_id = $1">>,
    case db_query:delete(Query, [GameId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

%% Local functions
