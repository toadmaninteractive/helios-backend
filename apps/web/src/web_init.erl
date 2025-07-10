-module(web_init).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("acl.hrl").
-include("settings.hrl").

%% Exported functions

-export([
    start_cowboy/4
]).

%% API

start_cowboy(BindIp, BindPort, _Acceptors, Secure) ->
    % Get paths, make up index and favicon
    WebDir = filename:join([filename:absname(""), "web"]),
    SslDir = filename:join([WebDir, "ssl"]),
    {ok, CaptchaDir} = web_config:captcha_dir(),
    FrontendDir = filename:join([WebDir, "frontend"]),
    AdminDistDir = filename:join([FrontendDir, "admin", "dist"]),
    MobileDistDir = filename:join([FrontendDir, "mobile", "dist"]),
    Index = "index.html",
    FavIcon = "favicon.ico",

    % Routes
    Routes = [
        % Administration panel
        {"/admin/assets/[...]", cowboy_static, {dir, filename:join([AdminDistDir, "assets"])}},
        {"/admin/static/[...]", cowboy_static, {dir, filename:join([AdminDistDir, "static"])}},
        {"/admin/dist/[...]", cowboy_static, {dir, AdminDistDir}},
        {"/admin/" ++ FavIcon, cowboy_static, {file, filename:join([AdminDistDir, FavIcon])}},
        {"/admin/[...]", cowboy_static, {file, filename:join([AdminDistDir, Index])}},
        {"/admin", cowboy_static, {file, filename:join([AdminDistDir, Index])}},
        {"/" ++ FavIcon, cowboy_static, {file, filename:join([AdminDistDir, FavIcon])}},

        % Mobile application
        {"/mobile/assets/[...]", cowboy_static, {dir, filename:join([MobileDistDir, "assets"])}},
        {"/mobile/static/[...]", cowboy_static, {dir, filename:join([MobileDistDir, "static"])}},
        {"/mobile/dist/[...]", cowboy_static, {dir, MobileDistDir}},
        {"/mobile/" ++ FavIcon, cowboy_static, {file, filename:join([MobileDistDir, FavIcon])}},
        {"/mobile/[...]", cowboy_static, {file, filename:join([MobileDistDir, Index])}},
        {"/mobile", cowboy_static, {file, filename:join([MobileDistDir, Index])}},

        % REST API: Administration
        {"/api/admin/branch/:id", web_rest_admin_branch, #acl{get = ?role_uploader, branch_id = id}},
        {"/api/admin/branch/:id/build", web_rest_admin_branch_build, #acl{put = ?role_uploader, branch_id = id}},
        {"/api/admin/branch/:id/default", web_rest_admin_branch_default, #acl{put = ?role_maintainer, branch_id = id}},
        {"/api/admin/branch/:id/:rev", web_rest_admin_branch_update, #acl{put = ?role_maintainer, branch_id = id}},
        {"/api/admin/branches/:guid", web_rest_admin_branches, #acl{get = ?role_uploader, post = ?role_maintainer, game_id = guid}},
        {"/api/admin/branches/:guid/all", web_rest_admin_branches_all, #acl{get = ?role_uploader, game_id = guid}},
        {"/api/admin/build/:id", web_rest_admin_build, #acl{get = ?role_uploader, delete = ?role_maintainer, build_id = id}},
        {"/api/admin/build/:guid/rev/:build_rev", web_rest_admin_build_rev, #acl{get = ?role_uploader, game_id = guid}},
        {"/api/admin/build/:id/files", web_rest_admin_build_files, #acl{get = ?role_uploader, delete = ?role_uploader, build_id = id}},
        {"/api/admin/build/:id/files/:file_id", web_rest_admin_build_file, #acl{delete = ?role_uploader, build_id = id}},
        {"/api/admin/build/:id/manifest", web_rest_admin_build_manifest, #acl{get = ?role_uploader, build_id = id}},
        {"/api/admin/build/:id/:rev", web_rest_admin_build_update, #acl{put = ?role_uploader, build_id = id}},
        {"/api/admin/build/:id/draft/publish", web_rest_admin_build_publish_draft, #acl{put = ?role_uploader, build_id = id}},
        {"/api/admin/build/:id/draft/upload", web_build_file_upload, #acl{post = ?role_uploader, build_id = id}},
        {"/api/admin/build/:id/draft/upload/archive", web_build_archive_upload, #acl{post = ?role_uploader, build_id = id}},
        {"/api/admin/build/:id/:rev/draft", web_rest_admin_build_update_draft, #acl{put = ?role_uploader, build_id = id}},
        {"/api/admin/builds", web_rest_admin_builds, #acl{get = ?role_superadmin}},
        {"/api/admin/builds/game/:guid", web_rest_admin_builds_game, #acl{get = ?role_uploader, game_id = guid}},
        {"/api/admin/builds/game/:guid/draft", web_rest_admin_build_create_draft, #acl{get = ?role_uploader, game_id = guid}},
        {"/api/admin/category/:id", web_rest_admin_game_category, #acl{delete = ?role_superadmin}},
        {"/api/admin/category/:id/:rev", web_rest_admin_game_category_update, #acl{put = ?role_maintainer}},
        {"/api/admin/categories", web_rest_admin_game_categories, #acl{post = ?role_superadmin}},
        {"/api/admin/client/:id", web_rest_admin_client, #acl{get = ?role_superadmin}},
        {"/api/admin/clients", web_rest_admin_clients, #acl{get = ?role_superadmin}},
        {"/api/admin/game/:guid", web_rest_admin_game, #acl{get = ?role_uploader, game_id = guid}},
        {"/api/admin/game/:guid/favourite", web_rest_admin_game_favourite, #acl{get = ?role_maintainer, put = ?role_maintainer, delete = ?role_maintainer, game_id = guid}},
        {"/api/admin/game/:guid/category/:category_id", web_rest_admin_game_category_manage, #acl{put = ?role_maintainer, delete = ?role_maintainer, game_id = guid}},
        {"/api/admin/game/:guid/:rev", web_rest_admin_game_update, #acl{put = ?role_maintainer, game_id = guid}},
        {"/api/admin/game/:guid/roles/account", web_rest_admin_game_roles_account, #acl{get = ?role_superadmin}},
        {"/api/admin/game/:guid/roles/group", web_rest_admin_game_roles_group, #acl{get = ?role_superadmin}},
        {"/api/admin/game/:guid/roles/me", web_rest_admin_game_roles_me, []},
        {"/api/admin/games", web_rest_admin_games, #acl{post = ?role_superadmin}},
        {"/api/admin/games/favourite", web_rest_admin_games_favourite, []},
        {"/api/admin/personnel/:id", web_rest_admin_personnel, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel/username/:username", web_rest_admin_personnel_username, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel/my/roles", web_rest_admin_personnel_my_roles, []},
        {"/api/admin/personnel/:id/roles", web_rest_admin_personnel_roles, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel/:id/roles/:guid", web_rest_admin_personnel_role_set, #acl{put = ?role_superadmin, delete = ?role_superadmin}},
        {"/api/admin/personnels", web_rest_admin_personnels, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel-groups/:id", web_rest_admin_personnel_group, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel-groups/name/:name", web_rest_admin_personnel_group_name, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel-groups/:id/roles", web_rest_admin_personnel_group_roles, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel-groups/:id/roles/:guid", web_rest_admin_personnel_group_role_set, #acl{put = ?role_superadmin, delete = ?role_superadmin}},
        {"/api/admin/personnel-groups", web_rest_admin_personnel_groups, #acl{get = ?role_superadmin}},
        {"/api/admin/settings", web_rest_admin_settings, #acl{get = ?role_superadmin, put = ?role_superadmin}},
        {"/api/admin/settings/ci_api_key", web_rest_admin_settings_ci_api_key, #acl{put = ?role_superadmin}},
        {"/api/admin/statistics/games/popular", web_rest_admin_popular_games, #acl{get = ?role_superadmin}},

        % REST API: Authentication and authorization for clients
        {"/api/auth/client/confirm", web_rest_auth_client_confirm, []},
        {"/api/auth/client/login", web_rest_auth_client_login, []},
        {"/api/auth/client/logout", web_rest_auth_client_logout, []},
        {"/api/auth/client/password/change", web_rest_auth_client_password_change, []},
        {"/api/auth/client/password/confirm", web_rest_auth_client_password_confirm, []},
        {"/api/auth/client/password/reset", web_rest_auth_client_password_reset, []},
        {"/api/auth/client/register", web_rest_auth_client_register, []},
        {"/api/auth/client/resend", web_rest_auth_client_resend, []},
        {"/api/auth/client/status", web_rest_auth_client_status, []},

        % REST API: Authentication and authorization for personnel
        {"/api/auth/personnel/api_key", web_rest_auth_personnel_api_key, []},
        {"/api/auth/personnel/login", web_rest_auth_personnel_login, []},
        {"/api/auth/personnel/logout", web_rest_auth_personnel_logout, []},
        {"/api/auth/personnel/profile", web_rest_auth_personnel_profile, []},
        {"/api/auth/personnel/status", web_rest_auth_personnel_status, []},

        % REST API: Selene
        {"/api/branches/:guid/unlock", web_rest_game_branch_unlock, []},
        {"/api/build/:guid/:build_rev", web_rest_game_build, []},
        {"/api/build/:guid/:build_rev/manifest", web_rest_game_build_manifest, []},
        {"/api/build/:guid/:build_rev/pdb", web_rest_game_build_pdb, []},
        {"/api/builds/:guid/:branch", web_rest_game_builds, []},

        {"/api/captcha", web_rest_captcha_request, []},
        {"/api/categories", web_rest_game_categories, []},
        {"/api/game/:guid", web_rest_game, []},
        {"/api/game/:guid/:branch/changes", web_rest_game_changes, []},
        {"/api/games", web_rest_game_list, []},
        {"/api/manifest/:guid/:branch", web_rest_game_manifest, []},

        % REST API: Helios Mobile
        {"/api/mobile/games", web_rest_mobile_games, []},
        {"/api/mobile/games/:guid/builds", web_rest_mobile_builds, []},

        % REST API: CI integration
        {"/api/ci/builds/:guid", web_rest_ci_builds_game, []},
        {"/api/ci/builds/:guid/upload", web_mobile_build_upload, []},

        % Stripe payments
        {"/stripe/charge", web_handler_stripe_charge, []},
        {"/stripe/form", web_handler_stripe_form, []},
        {"/stripe/static/[...]", cowboy_static, {dir, filename:join([WebDir, "stripe"])}},

        % CAPTCHA
        {"/captcha/[...]", cowboy_static, {dir, CaptchaDir}},
        {"/recaptcha", web_handler_recaptcha, []},

        % Websocket
        {"/ws/personnel", web_ws_personnel, []},

        % Catch-up
        {'_', web_handler_redirect, []}
        % {'_', web_handler_not_found, []}
    ],

    % Compile route dispatcher
    Dispatch = cowboy_router:compile([{'_', Routes}]),

    % Define middlewares
    Middlewares = [
        % web_mw_log,
        web_mw_allow_origin,
        web_mw_no_cache,
        web_mw_auth,
        % web_mw_websocket,
        cowboy_router,
        web_mw_acl,
        cowboy_handler
    ],

    % Define SSL options
    SslOpts = get_ssl_opt(cacertfile, SslDir) ++ get_ssl_opt(certfile, SslDir) ++ get_ssl_opt(keyfile, SslDir),

    % Define server starter, options and environment
    StarterFun = ?yesno(Secure, fun cowboy:start_tls/3, fun cowboy:start_clear/3),
    % Opts = #{ip => BindIp, port => BindPort, num_acceptors => Acceptors},
    Opts = [{ip, BindIp}, {port, BindPort}],
    OptsMaybeWithSsl = ?yesno(Secure, Opts ++ SslOpts, Opts),

    % Specify protocol options
    ProtocolOpts = #{
        env => #{
            dispatch => Dispatch,
            auth_prefixes => [<<"/api/">>, <<"/ws/">>]
        },
        middlewares => Middlewares
    },

    % Start server
    StarterFun(helios_web_server, OptsMaybeWithSsl, ProtocolOpts).

%% Local functions

absolute_or_local(FilePath, LocalSslDir) ->
    case filelib:is_regular(FilePath) of
        true -> FilePath;
        false -> filename:join([LocalSslDir, FilePath])
    end.

get_ssl_opt(Param, LocalSslDir) ->
    Result = case Param of
        cacertfile -> web_config:cacertfile();
        certfile -> web_config:certfile();
        keyfile -> web_config:keyfile();
        _ -> undefined
    end,
    case Result of
        {ok, Value} -> [{Param, absolute_or_local(Value, LocalSslDir)}];
        undefined -> []
    end.
