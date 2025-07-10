-module(db_if_clients).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get_one_by_username/1,
    get_one_by_email/1,
    get/5,
    get_count/1,
    create/3,
    update/2,
    block/1,
    unblock/1,
    delete/1,
    undelete/1,
    credentials/1,
    credentials_by_username/1,
    is_activated/1,
    activate/1,
    profile/1,
    update_profile/2,
    update_password/2,
    update_password_hash/3,
    client_id_by_username/1,
    client_id_by_email/1,
    email_exists/1,
    username_exists/1
]).

%% API

-spec get_one(ClientId :: non_neg_integer()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one(ClientId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM clients AS cli ",
        (common_joins())/binary,
        "WHERE cli.id = $1"
    >>,
    case db_query:select_one(Query, [ClientId]) of
        {ok, Account} -> {ok, Account};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_one_by_username(Username :: binary()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one_by_username(Username) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM clients AS cli ",
        (common_joins())/binary,
        "WHERE cli.username = lower(trim($1))"
    >>,
    case db_query:select_one(Query, [Username]) of
        {ok, Account} -> {ok, Account};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_one_by_email(Email :: binary()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one_by_email(Email) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM clients AS cli ",
        (common_joins())/binary,
        "WHERE cli.email = lower(trim($1))"
    >>,
    case db_query:select_one(Query, [Email]) of
        {ok, Account} -> {ok, Account};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get(Needle, OrderBy, OrderDir, Offset, Limit) -> Result when
    Needle :: binary() | 'undefined',
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(), Limit :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get(Needle, OrderBy, OrderDir, Offset, Limit) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<"WHERE strpos(LOWER(cli.username), LOWER($1)) > 0 OR strpos(LOWER(cli.email), LOWER($1)) > 0">>, <<>>),
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM clients AS cli ",
        (common_joins())/binary,
        NeedlePart/binary,
        Filter/binary
    >>,
    Params = ?yesno(HasNeedle, [Needle], []),
    db_query:select(Query, Params).

-spec get_count(Needle) -> Result when
    Needle :: binary() | 'undefined',
    Result :: {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_count(Needle) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<"WHERE strpos(LOWER(cli.username), LOWER($1)) > 0 OR strpos(LOWER(cli.email), LOWER($1)) > 0">>, <<>>),
    Query = <<
        "SELECT COUNT(*)::bigint AS count ",
        "FROM clients AS cli ",
        NeedlePart/binary
    >>,
    Params = ?yesno(HasNeedle, [Needle], []),
    case db_query:select_one(Query, Params) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec create(Username :: binary(), Password :: binary(), Email :: binary()) ->
    {'ok', ClientId :: non_neg_integer()} | {error, Reason :: atom()}.

create(Username, Password, Email) ->
    Salt = db_util:salt(),
    PasswordHash = db_util:password_hash(Password, Salt),
    Params = [Username, PasswordHash, Salt, Email],
    Query = <<
        "INSERT INTO clients (username, password, salt, email) ",
        "VALUES (lower(trim($1)), $2, $3, lower(trim($4))) ",
        "RETURNING id::bigint"
    >>,
    case db_query:insert(Query, Params) of
        {ok, 1, Columns, Rows} ->
            [#{<<"id">> := ClientId}] = db_util:result_to_json(Columns, Rows),
            {ok, ClientId};
        {error, unique_violation} ->
            {error, ?err_already_exists};
        {error, Other} ->
            {error, Other}
    end.

-spec update(ClientId :: non_neg_integer(), Patch :: maps:map()) ->
    'ok' | {'error', Reason :: atom()}.

update(ClientId, Patch) ->
    % Define all possible fields
    Fields = [
        ?mk_mod_lower_trim(username),
        password,
        salt,
        ?mk_mod_lower_trim(email),
        ?mk_mod_lower_trim(phone),
        ?mk_mod_trim(register_confirm_code),
        ?mk_mod_trim(password_reset_code),
        is_activated,
        is_blocked,
        is_deleted
    ],

    % Define auto-set fields
    AutoSetFields = [
        ?mk_mod_inc(rev),
        ?mk_mod_set_now(updated_at)
    ],

    % Perform update
    case db_util:mk_update(<<"clients">>, <<"id">>, ClientId, Fields, AutoSetFields, Patch) of
        {ok, Query, Params} ->
            case db_query:update(Query, Params) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, ?err_not_exists};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec block(ClientId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

block(ClientId) ->
    update(ClientId, #{<<"is_blocked">> => true}).

-spec unblock(ClientId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

unblock(ClientId) ->
    update(ClientId, #{<<"is_blocked">> => false}).

-spec delete(ClientId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

delete(ClientId) ->
    update(ClientId, #{<<"is_deleted">> => true}).

-spec undelete(ClientId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

undelete(ClientId) ->
    update(ClientId, #{<<"is_deleted">> => false}).

-spec credentials(ClientId :: non_neg_integer()) ->
    {'ok', jsx:json_term()}
    | {'error', Reason :: atom()}.

credentials(ClientId) ->
    Query = <<
        "SELECT ", (credential_fields())/binary,
        "FROM clients AS cli ",
        "WHERE cli.id = $1"
    >>,
    case db_query:select_one(Query, [ClientId]) of
        {ok, Credentials} -> {ok, Credentials};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec credentials_by_username(Username :: binary()) ->
    {'ok', jsx:json_term()}
    | {'error', Reason :: atom()}.

credentials_by_username(Username) ->
    Query = <<
        "SELECT ", (credential_fields())/binary,
        "FROM clients AS cli ",
        "WHERE cli.username = lower(trim($1))"
    >>,
    case db_query:select_one(Query, [Username]) of
        {ok, Credentials} -> {ok, Credentials};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec is_activated(ClientId :: non_neg_integer()) ->
    {'ok', boolean()} | {'error', Reason :: atom()}.

is_activated(ClientId) ->
    Query = <<"SELECT is_activated FROM clients WHERE id = $1">>,
    case db_query:select_one(Query, [ClientId]) of
        {ok, #{<<"is_activated">> := IsActivated}} -> {ok, IsActivated};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec activate(ClientId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

activate(ClientId) ->
    update(ClientId, #{<<"is_activated">> => true}).

-spec profile(ClientId :: non_neg_integer()) ->
    {'ok', Account :: jsx:json_term()} | {'error', Reason :: atom()}.

profile(ClientId) ->
    Query = <<
        "SELECT ", (profile_fields())/binary,
        "FROM clients AS cli ",
        (common_joins())/binary,
        "WHERE cli.id = $1"
    >>,
    case db_query:select_one(Query, [ClientId]) of
        {ok, Profile} -> {ok, Profile};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec update_profile(ClientId :: non_neg_integer(), Patch :: maps:map()) ->
    'ok' | {'error', Reason :: atom()}.

update_profile(ClientId, Patch) ->
    update(ClientId, db_util:get_patch([email, phone], Patch)).

-spec update_password(ClientId :: non_neg_integer(), Password :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

update_password(ClientId, Password) when is_binary(Password) ->
    Salt = db_util:salt(),
    PasswordHash = db_util:password_hash(Password, Salt),
    update(ClientId, #{<<"salt">> => Salt, <<"password">> => PasswordHash}).

-spec update_password_hash(ClientId :: non_neg_integer(), Salt :: binary(), PasswordHash :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

update_password_hash(ClientId, Salt, PasswordHash) when is_binary(Salt), is_binary(PasswordHash) ->
    update(ClientId, #{<<"salt">> => Salt, <<"password">> => PasswordHash}).

-spec client_id_by_username(Username :: binary()) ->
    {'ok', ClientId :: non_neg_integer()} | {'error', Reason :: atom()}.

client_id_by_username(Username) ->
    Query = <<"SELECT id FROM clients WHERE username = lower(trim($1))">>,
    case db_query:select_one(Query, [Username]) of
        {ok, #{<<"id">> := ClientId}} -> {ok, ClientId};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec client_id_by_email(Email :: binary()) ->
    {'ok', ClientId :: non_neg_integer()} | {'error', Reason :: atom()}.

client_id_by_email(Email) ->
    Query = <<"SELECT id FROM clients WHERE email = lower(trim($1))">>,
    case db_query:select_one(Query, [Email]) of
        {ok, #{<<"id">> := ClientId}} -> {ok, ClientId};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec email_exists(Email :: binary()) ->
    {'ok', boolean()} | {'error', Reason :: atom()}.

email_exists(Email) ->
    Query = <<"SELECT count(id)::bigint FROM clients WHERE email = lower(trim($1))">>,
    case db_query:select_one(Query, [Email]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count > 0};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec username_exists(Username :: binary()) ->
    {'ok', boolean()} | {'error', Reason :: atom()}.

username_exists(Username) ->
    Query = <<"SELECT count(id)::bigint FROM clients WHERE username = lower(trim($1))">>,
    case db_query:select_one(Query, [Username]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count > 0};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % cli : clients
    <<
        "cli.id AS id, ",
        "cli.rev AS rev, ",
        "cli.username AS username, ",
        "cli.email AS email, ",
        "cli.phone AS phone, ",
        "cli.is_activated AS is_activated, "
        "cli.is_blocked AS is_blocked, ",
        "cli.is_deleted AS is_deleted, ",
        "cli.created_at AS created_at, ",
        "cli.updated_at AS updated_at "
    >>.

credential_fields() ->
    % Aliases:
    % cli : clients
    <<
        "cli.id AS id, ",
        "cli.password AS password, ",
        "cli.salt AS salt, "
        "cli.is_activated AS is_activated, "
        "cli.is_blocked AS is_blocked, "
        "cli.is_deleted AS is_deleted "
    >>.

profile_fields() ->
    % Aliases:
    % cli : clients
    <<
        "cli.id AS id, ",
        "cli.username AS username, ",
        "cli.email AS email, ",
        "cli.phone AS phone "
    >>.

common_joins() ->
    <<>>.
