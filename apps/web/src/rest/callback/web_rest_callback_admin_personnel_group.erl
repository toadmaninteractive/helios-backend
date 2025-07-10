-module(web_rest_callback_admin_personnel_group).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("web_protocol.hrl").
-include("session.hrl").
-include("limits.hrl").

%% Exported functions

-export([
    get_personnel_group/2,
    get_personnel_group_by_name/2,
    get_personnel_groups/6
]).

%% API

-spec get_personnel_group(Id, Req) -> Response when
    Id :: integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:personnel_group(), cowboy_req:req()}.

get_personnel_group(Id, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, PersonnelGroup} = db_if_personnel_groups:get_one(Id),
    {web_protocol:personnel_group_from_json(PersonnelGroup), Req};

get_personnel_group(_Id, _Req) ->
    % Unauthorized
    web_rest_admin_personnel_group:get_personnel_group_403(#forbidden_error{}).

-spec get_personnel_group_by_name(Name, Req) -> Response when
    Name :: binary(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:personnel_group(), cowboy_req:req()}.

get_personnel_group_by_name(Name, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, PersonnelGroup} = db_if_personnel_groups:get_one_by_name(Name),
    {web_protocol:personnel_group_from_json(PersonnelGroup), Req};

get_personnel_group_by_name(_Name, _Req) ->
    % Unauthorized
    web_rest_admin_personnel_group_name:get_personnel_group_by_name_403(#forbidden_error{}).

-spec get_personnel_groups(Needle, OrderBy, OrderDir, Offset, Limit, Req) -> Response when
    Needle :: binary() | 'undefined',
    OrderBy :: web_protocol:personnel_group_order_by(),
    OrderDir :: web_protocol:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Response :: {web_protocol:collection_slice(web_protocol:personnel_group()), cowboy_req:req()}.

get_personnel_groups(Needle, OrderBy, OrderDir, Offset, Limit, #{?m_session := #session{key = {?actor_personnel, _}, user_id = _UserId}} = Req) ->
    {ok, PersonnelGroups} = db_if_personnel_groups:get(Needle, util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit)),
    {ok, Total} = db_if_personnel_groups:get_count(Needle),
    PersonnelGroups1 = lists:map(fun web_protocol:personnel_group_from_json/1, PersonnelGroups),
    {#collection_slice{items = PersonnelGroups1, total = Total}, Req};

get_personnel_groups(_Needle, _OrderBy, _OrderDir, _Offset, _Limit, _Req) ->
    % Unauthorized
    web_rest_admin_personnel_groups:get_personnel_groups_403(#forbidden_error{}).

%% Local functions
