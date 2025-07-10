% Roles
-define(role_consumer, consumer).
-define(role_uploader, uploader).
-define(role_maintainer, maintainer).
-define(role_admin, admin).
-define(role_superadmin, superadmin).

-record(acl, {
    game_id :: atom() | 'undefined',
    branch_id :: atom() | 'undefined',
    build_id :: atom() | 'undefined',
    get :: web_protocol:access_role() | 'undefined',
    post :: web_protocol:access_role() | 'undefined',
    put :: web_protocol:access_role() | 'undefined',
    patch :: web_protocol:access_role() | 'undefined',
    delete :: web_protocol:access_role() | 'undefined'
}).

-type acl() :: #acl{}.