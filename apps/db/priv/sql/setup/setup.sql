-- Setup SQL
-- Revision: 49

-------------------
-- Custom types  --
-------------------

CREATE TYPE actor_t AS ENUM (
    'root',
    'personnel',
    'client',
    'robot',
    'anonymous'
);

CREATE TYPE entity_t AS ENUM (
    'settings',
    'personnel',
    'client',
    'game',
    'game_build',
    'game_manifest',
    'game_branch',
    'game_branch_unlock',
    'game_ownership'
);

CREATE TYPE operation_t AS ENUM (
    'create',
    'read',
    'update',
    'delete',
    'undelete',
    'block',
    'unblock',
    'register',
    'register_confirm',
    'phone_confirm',
    'password_reset',
    'password_reset_confirm',
    'login',
    'logout',
    'expire',
    'purchase',
    'grant',
    'revoke'
);

CREATE TYPE request_t AS ENUM (
    'register_confirm',
    'phone_confirm',
    'password_reset_confirm'
);

CREATE TYPE ownership_t AS ENUM (
    'none',
    'purchase',
    'grant',
    'employee'
);

CREATE TYPE platform_t AS ENUM (
    'windows',
    'linux',
    'macos',
    'ios',
    'android'
);

CREATE TYPE role_t AS ENUM (
    'consumer',
    'uploader',
    'maintainer',
    'admin'
);

------------------------
-- Tables and indexes --
------------------------

-- Properties
CREATE TABLE props
(
    name varchar PRIMARY KEY CHECK (TRIM(name) <> ''),
    value varchar
) WITHOUT OIDS;

-- Settings
CREATE TABLE settings
(
    param varchar PRIMARY KEY CHECK (TRIM(param) <> ''),
    type varchar NOT NULL CHECK (TRIM(type) <> ''),
    value text DEFAULT NULL
) WITHOUT OIDS;

INSERT INTO settings ("param", "type", "value") VALUES 
    ('personnel_session_duration', 'int', '2592000'),
    ('client_session_duration', 'int', '2592000'),
    ('register_confirm_code_lifetime', 'int', '604800'),
    ('phone_confirm_code_lifetime', 'int', '600'),
    ('password_reset_confirm_code_lifetime', 'int', '600'),
    ('ci_api_key', 'string', NULL);

-- Logs
CREATE TABLE logs
(
    id bigserial PRIMARY KEY,
    actor actor_t NOT NULL,
    actor_id bigint DEFAULT NULL,
    entity entity_t NOT NULL,
    entity_id bigint DEFAULT NULL,
    operation operation_t NOT NULL,
    properties jsonb DEFAULT '{}',
    result boolean NOT NULL DEFAULT FALSE,
    created_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE INDEX log_actor_index ON logs (actor);
CREATE INDEX log_actor_id_index ON logs (actor_id);
CREATE INDEX log_entity_index ON logs (entity);
CREATE INDEX log_entity_id_index ON logs (entity_id);
CREATE INDEX log_operation_index ON logs (operation);
CREATE INDEX log_result_index ON logs (result);
CREATE INDEX log_created_at_index ON logs (created_at);

-- Personnel accounts
CREATE TABLE personnel
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    username varchar UNIQUE NOT NULL CHECK (TRIM(username) <> ''),
    name varchar DEFAULT NULL,
    email varchar DEFAULT NULL CHECK (TRIM(email) <> ''),
    phone varchar DEFAULT NULL CHECK (TRIM(phone) <> ''),
    api_key varchar DEFAULT NULL,
    is_blocked boolean NOT NULL DEFAULT FALSE,
    is_deleted boolean NOT NULL DEFAULT FALSE,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX per_username_ult_index ON personnel (LOWER(TRIM(username)));
CREATE UNIQUE INDEX per_email_ult_index ON personnel (LOWER(TRIM(email)));
CREATE UNIQUE INDEX per_phone_ult_index ON personnel (LOWER(TRIM(phone)));
CREATE UNIQUE INDEX per_api_key_ut_index ON personnel (TRIM(api_key)) WHERE api_key IS NOT NULL;
CREATE INDEX per_is_blocked_index ON personnel (is_blocked);
CREATE INDEX per_is_deleted_index ON personnel (is_deleted);
CREATE INDEX per_created_at_index ON personnel (created_at);
CREATE INDEX per_updated_at_index ON personnel (updated_at);

-- Personnel sessions
CREATE TABLE personnel_sessions
(
    id varchar PRIMARY KEY CHECK (TRIM(id) <> ''),
    personnel_id bigint NOT NULL REFERENCES personnel (id),
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    valid_thru timestamptz NOT NULL
) WITHOUT OIDS;

CREATE INDEX pers_personnel_id_index ON personnel_sessions (personnel_id);
CREATE INDEX pers_created_at_index ON personnel_sessions (created_at);
CREATE INDEX pers_valid_thru_index ON personnel_sessions (valid_thru);

-- Personnel groups
CREATE TABLE personnel_groups
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    name varchar UNIQUE NOT NULL CHECK (TRIM(name) <> ''),
    description varchar DEFAULT NULL,
    is_superadmin boolean NOT NULL DEFAULT FALSE,
    is_deleted boolean NOT NULL DEFAULT FALSE,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX perg_name_ult_index ON personnel_groups (LOWER(TRIM(name)));
CREATE INDEX perg_is_superadmin_index ON personnel_groups (is_superadmin);
CREATE INDEX perg_is_deleted_index ON personnel_groups (is_deleted);
CREATE INDEX perg_created_at_index ON personnel_groups (created_at);
CREATE INDEX perg_updated_at_index ON personnel_groups (updated_at);

-- Personnel group membership
CREATE TABLE personnel_group_membership
(
    personnel_id bigint NOT NULL REFERENCES personnel (id),
    group_id bigint NOT NULL REFERENCES personnel_groups (id),
    PRIMARY KEY (personnel_id, group_id)
) WITHOUT OIDS;

CREATE INDEX pergm_personnel_id_index ON personnel_group_membership (personnel_id);
CREATE INDEX pergm_group_id_index ON personnel_group_membership (group_id);

-- Client accounts
CREATE TABLE clients
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    username varchar UNIQUE NOT NULL CHECK (TRIM(username) <> ''),
    password varchar NOT NULL,
    salt varchar NOT NULL,
    email varchar UNIQUE NOT NULL CHECK (TRIM(email) <> ''),
    phone varchar UNIQUE DEFAULT NULL CHECK (TRIM(phone) <> ''),
    is_activated boolean NOT NULL DEFAULT FALSE,
    is_blocked boolean NOT NULL DEFAULT FALSE,
    is_deleted boolean NOT NULL DEFAULT FALSE,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX cli_username_ult_index ON clients (LOWER(TRIM(username)));
CREATE UNIQUE INDEX cli_email_ult_index ON clients (LOWER(TRIM(email)));
CREATE UNIQUE INDEX cli_phone_ult_index ON clients (LOWER(TRIM(phone)));
CREATE INDEX cli_is_activated_index ON clients (is_activated);
CREATE INDEX cli_is_blocked_index ON clients (is_blocked);
CREATE INDEX cli_is_deleted_index ON clients (is_deleted);
CREATE INDEX cli_created_at_index ON clients (created_at);
CREATE INDEX cli_updated_at_index ON clients (updated_at);

-- Client sessions
CREATE TABLE client_sessions
(
    id varchar PRIMARY KEY CHECK (TRIM(id) <> ''),
    client_id bigint NOT NULL REFERENCES clients (id),
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    valid_thru timestamptz NOT NULL
) WITHOUT OIDS;

CREATE INDEX clis_client_id_index ON client_sessions (client_id);
CREATE INDEX clis_created_at_index ON client_sessions (created_at);
CREATE INDEX clis_valid_thru_index ON client_sessions (valid_thru);

-- Client requests confirmed by security codes
CREATE TABLE client_requests
(
    client_id bigint NOT NULL REFERENCES clients (id),
    request request_t NOT NULL,
    security_code varchar NOT NULL,
    properties jsonb DEFAULT '{}',
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    valid_thru timestamptz NOT NULL,
    PRIMARY KEY (client_id, request)
) WITHOUT OIDS;

CREATE INDEX clir_client_id_index ON client_requests (client_id);
CREATE INDEX clir_request_index ON client_requests (request);
CREATE INDEX clir_created_at_index ON client_requests (created_at);
CREATE INDEX clir_valid_thru_index ON client_requests (valid_thru);

-- Games
CREATE TABLE games
(
    id varchar PRIMARY KEY CHECK (TRIM(id) <> ''),
    rev integer NOT NULL DEFAULT 1,
    title varchar NOT NULL CHECK (TRIM(title) <> ''),
    description varchar NOT NULL DEFAULT '',
    jira_key varchar DEFAULT NULL,
    selene_key varchar DEFAULT NULL,
    ci_url varchar DEFAULT NULL,
    discord_url varchar DEFAULT NULL,
    price numeric(15, 6) NOT NULL DEFAULT 0 CHECK (price >= 0),
    currency varchar NOT NULL,
    build_lifetime integer NOT NULL DEFAULT 0 CHECK (build_lifetime >= 0),
    is_published boolean NOT NULL DEFAULT FALSE,
    is_disabled boolean NOT NULL DEFAULT FALSE,
    is_deleted boolean NOT NULL DEFAULT FALSE,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX game_title_ult_index ON games (LOWER(TRIM(title)));
CREATE INDEX game_jira_key_ut_index ON games (UPPER(TRIM(jira_key)));
CREATE INDEX game_selene_key_ut_index ON games (UPPER(TRIM(selene_key)));
CREATE INDEX game_build_lifetime_index ON games (build_lifetime);
CREATE INDEX game_is_published_index ON games (is_published);
CREATE INDEX game_is_disabled_index ON games (is_disabled);
CREATE INDEX game_is_deleted_index ON games (is_deleted);
CREATE INDEX game_created_at_index ON games (created_at);
CREATE INDEX game_updated_at_index ON games (updated_at);

-- Game builds
CREATE TABLE game_builds
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    game_id varchar NOT NULL REFERENCES games (id),
    build_rev varchar NOT NULL CHECK (TRIM(build_rev) <> ''),
    commentary varchar NOT NULL DEFAULT '',
    change_list varchar NOT NULL DEFAULT '',
    total_size bigint NOT NULL CHECK (total_size >= 0),
    compressed_size bigint NOT NULL CHECK (compressed_size >= 0),
    exe_path varchar NOT NULL DEFAULT '',
    log_path varchar NOT NULL DEFAULT '',
    crash_report_path varchar NOT NULL DEFAULT '',
    config_path varchar NOT NULL DEFAULT '',
    optional_file_masks jsonb DEFAULT '[]',
    preserved_file_masks jsonb DEFAULT '[]',
    redistributables jsonb DEFAULT '[]',
    pdb_files jsonb DEFAULT '[]',
    cdn_root_url varchar NOT NULL CHECK (TRIM(cdn_root_url) <> ''),
    platform platform_t NOT NULL DEFAULT 'windows',
    is_local boolean NOT NULL DEFAULT TRUE,
    is_permanent boolean NOT NULL DEFAULT FALSE,
    is_draft boolean NOT NULL DEFAULT FALSE,
    is_processing boolean NOT NULL DEFAULT FALSE,
    archived_size bigint NOT NULL DEFAULT 0 CHECK (archived_size >= 0),
    processed_size bigint NOT NULL DEFAULT 0 CHECK (processed_size >= 0),
    is_deleted boolean NOT NULL DEFAULT FALSE,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX gbl_game_build_uc_index ON game_builds (game_id, build_rev);
CREATE INDEX gbl_game_id_index ON game_builds (game_id);
CREATE INDEX gbl_total_size_index ON game_builds (total_size);
CREATE INDEX gbl_compressed_size_index ON game_builds (compressed_size);
CREATE INDEX gbl_platform_index ON game_builds (platform);
CREATE INDEX gbl_is_local_index ON game_builds (is_local);
CREATE INDEX gbl_is_permanent_index ON game_builds (is_permanent);
CREATE INDEX gbl_is_draft_index ON game_builds (is_draft);
CREATE INDEX gbl_is_deleted_index ON game_builds (is_deleted);
CREATE INDEX gbl_created_at_index ON game_builds (created_at);
CREATE INDEX gbl_updated_at_index ON game_builds (updated_at);

-- Game manifests (for live builds)
CREATE TABLE game_manifests
(
    build_id bigint PRIMARY KEY REFERENCES game_builds (id),
    manifest jsonb DEFAULT '{}',
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE INDEX gmn_created_at_index ON game_manifests (created_at);
CREATE INDEX gmn_updated_at_index ON game_manifests (updated_at);

-- Game build files (for draft builds)
CREATE TABLE game_build_files
(
    id bigserial PRIMARY KEY,
    build_id bigint NOT NULL REFERENCES game_builds (id),
    file_path varchar NOT NULL,
    file_size bigint NOT NULL CHECK (file_size >= 0),
    compressed_file_path varchar NOT NULL,
    compressed_file_size bigint NOT NULL CHECK (compressed_file_size >= 0),
    md5 varchar NOT NULL,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX gblf_build_file_uc_index ON game_build_files (build_id, file_path);
CREATE INDEX gblf_build_id_index ON game_build_files (build_id);
CREATE INDEX gblf_file_path_index ON game_build_files (file_path);
CREATE INDEX gblf_file_size_index ON game_build_files (file_size);
CREATE INDEX gblf_compressed_file_path_index ON game_build_files (compressed_file_path);
CREATE INDEX gblf_compressed_file_size_index ON game_build_files (compressed_file_size);
CREATE INDEX gblf_created_at_index ON game_build_files (created_at);
CREATE INDEX gblf_updated_at_index ON game_build_files (updated_at);

-- Game branches
CREATE TABLE game_branches
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    game_id varchar NOT NULL REFERENCES games (id),
    build_id bigint DEFAULT NULL REFERENCES game_builds (id),
    title varchar NOT NULL CHECK (TRIM(title) <> ''),
    description varchar NOT NULL DEFAULT '',
    password varchar NOT NULL DEFAULT '',
    game_engine varchar DEFAULT NULL CHECK (TRIM(game_engine) <> ''),
    platform platform_t NOT NULL DEFAULT 'windows',
    ini_config jsonb DEFAULT '[]',
    registry_config jsonb DEFAULT '[]',
    is_reportable boolean NOT NULL DEFAULT FALSE,
    is_public boolean NOT NULL DEFAULT FALSE,
    is_default boolean NOT NULL DEFAULT FALSE,
    is_deleted boolean NOT NULL DEFAULT FALSE,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX gbr_game_branch_uc_index ON game_branches (game_id, LOWER(TRIM(title)));
CREATE UNIQUE INDEX gbr_platform_default_index ON game_branches (game_id, platform, is_default) WHERE is_default;
CREATE INDEX gbr_game_id_index ON game_branches (game_id);
CREATE INDEX gbr_build_id_index ON game_branches (build_id);
CREATE INDEX gbr_game_engine_index ON game_branches (game_engine);
CREATE INDEX gbr_platform_index ON game_branches (platform);
CREATE INDEX gbr_is_reportable_index ON game_branches (is_reportable);
CREATE INDEX gbr_is_public_index ON game_branches (is_public);
CREATE INDEX gbr_is_default_index ON game_branches (is_default);
CREATE INDEX gbr_is_deleted_index ON game_branches (is_deleted);
CREATE INDEX gbr_created_at_index ON game_branches (created_at);
CREATE INDEX gbr_updated_at_index ON game_branches (updated_at);

-- Game branch assignments
CREATE TABLE game_branch_assignments
(
    branch_id bigint NOT NULL REFERENCES game_branches (id) ON DELETE CASCADE,
    build_id bigint NOT NULL REFERENCES game_builds (id) ON DELETE CASCADE,
    assigned_at timestamptz NOT NULL DEFAULT current_timestamp,
    PRIMARY KEY (branch_id, build_id)
) WITHOUT OIDS;

CREATE INDEX gbra_branch_id_index ON game_branch_assignments (branch_id);
CREATE INDEX gbra_build_id_index ON game_branch_assignments (build_id);
CREATE INDEX gbra_assign_desc_index ON game_branch_assignments (branch_id, assigned_at DESC);

-- Game branch unlocks
CREATE TABLE game_branch_unlocks
(
    branch_id bigint NOT NULL REFERENCES game_branches (id),
    client_id bigint NOT NULL REFERENCES clients (id),
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    PRIMARY KEY (branch_id, client_id)
) WITHOUT OIDS;

CREATE INDEX gbu_branch_id_index ON game_branch_unlocks (branch_id);
CREATE INDEX gbu_client_id_index ON game_branch_unlocks (client_id);
CREATE INDEX gbu_created_at_index ON game_branch_unlocks (created_at);

-- Game ownership
CREATE TABLE game_ownership
(
    game_id varchar NOT NULL REFERENCES games (id),
    client_id bigint NOT NULL REFERENCES clients (id),
    ownership ownership_t NOT NULL,
    properties jsonb DEFAULT '{}',
    valid_thru timestamptz DEFAULT NULL,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp,
    PRIMARY KEY (game_id, client_id)
) WITHOUT OIDS;

CREATE INDEX go_game_id_index ON game_ownership (game_id);
CREATE INDEX go_client_id_index ON game_ownership (client_id);
CREATE INDEX go_ownership_index ON game_ownership (ownership);
CREATE INDEX go_valid_thru_index ON game_ownership (valid_thru);
CREATE INDEX go_created_at_index ON game_ownership (created_at);
CREATE INDEX go_updated_at_index ON game_ownership (updated_at);

-- Game categories
CREATE TABLE game_categories
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    name varchar NOT NULL CHECK (TRIM(name) <> ''),
    description varchar NOT NULL DEFAULT '',
    sort_order integer NOT NULL DEFAULT 1,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX gc_name_ult_index ON game_categories (LOWER(TRIM(name)));

-- Game category assignments
CREATE TABLE game_category_assignments
(
    game_id varchar NOT NULL REFERENCES games (id),
    category_id bigint NOT NULL REFERENCES game_categories (id) ON DELETE CASCADE,
    assigned_at timestamptz NOT NULL DEFAULT current_timestamp,
    PRIMARY KEY (game_id, category_id)
) WITHOUT OIDS;

CREATE INDEX gca_game_id_index ON game_category_assignments (game_id);
CREATE INDEX gca_category_id_index ON game_category_assignments (category_id);

-- Personnel favourite games
CREATE TABLE personnel_favourite_games
(
    personnel_id bigint NOT NULL REFERENCES personnel (id) ON DELETE CASCADE,
    game_id varchar NOT NULL REFERENCES games (id) ON DELETE CASCADE,
    PRIMARY KEY (personnel_id, game_id)
) WITHOUT OIDS;

CREATE INDEX perfg_personnel_id_index ON personnel_favourite_games (personnel_id);
CREATE INDEX perfg_game_id_index ON personnel_favourite_games (game_id);

-- Personnel roles
CREATE TABLE personnel_roles
(
    personnel_id bigint NOT NULL REFERENCES personnel (id),
    game_id varchar NOT NULL REFERENCES games (id),
    role role_t NOT NULL,
    is_global boolean NOT NULL DEFAULT TRUE,
    branch_ids jsonb NOT NULL DEFAULT '[]',
    PRIMARY KEY (personnel_id, game_id)
) WITHOUT OIDS;

CREATE INDEX perr_personnel_id_index ON personnel_roles (personnel_id);
CREATE INDEX perr_game_id_index ON personnel_roles (game_id);
CREATE INDEX perr_role_index ON personnel_roles (role);
CREATE INDEX perr_is_global_index ON personnel_roles (is_global);
CREATE INDEX perr_branch_ids_gin_index ON personnel_roles USING gin (branch_ids);

-- Personnel group roles
CREATE TABLE personnel_group_roles
(
    group_id bigint NOT NULL REFERENCES personnel_groups (id),
    game_id varchar NOT NULL REFERENCES games (id),
    role role_t NOT NULL,
    is_global boolean NOT NULL DEFAULT TRUE,
    branch_ids jsonb NOT NULL DEFAULT '[]',
    PRIMARY KEY (group_id, game_id)
) WITHOUT OIDS;

CREATE INDEX pergr_group_id_index ON personnel_group_roles (group_id);
CREATE INDEX pergr_game_id_index ON personnel_group_roles (game_id);
CREATE INDEX pergr_role_index ON personnel_group_roles (role);
CREATE INDEX pergr_is_global_index ON personnel_group_roles (is_global);
CREATE INDEX pergr_branch_ids_gin_index ON personnel_group_roles USING gin (branch_ids);

-------------------
-- SQL functions --
-------------------

-- Set default branch
CREATE OR REPLACE FUNCTION set_default_branch(_branch_id bigint) RETURNS text AS $$
DECLARE
    p_branch game_branches;
BEGIN
    -- Get game branch and check it
    SELECT * INTO p_branch FROM game_branches WHERE id = _branch_id;
    IF (p_branch.id IS NULL OR p_branch.is_deleted) THEN RETURN 'branch_not_exists'; END IF;

    -- Reset default branches for given game and platform
    UPDATE game_branches SET is_default = FALSE WHERE game_id = p_branch.game_id AND platform = p_branch.platform;

    -- Set default branch
    UPDATE game_branches SET is_default = TRUE WHERE id = _branch_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Assign build to game branch
CREATE OR REPLACE FUNCTION assign_build_to_branch(_branch_id bigint, _build_id bigint) RETURNS text AS $$
DECLARE
    p_branch game_branches;
    p_build game_builds;
BEGIN
    -- Get game branch and check it
    SELECT * INTO p_branch FROM game_branches WHERE id = _branch_id;
    IF (p_branch.id IS NULL OR p_branch.is_deleted) THEN RETURN 'branch_not_exists'; END IF;

    -- Get game build and check it
    SELECT * INTO p_build FROM game_builds WHERE id = _build_id;
    IF (p_build.id IS NULL OR p_build.is_deleted) THEN RETURN 'build_not_exists'; END IF;
    IF (p_build.is_draft) THEN RETURN 'build_is_draft'; END IF;

    -- Both game_id and platform should match
    IF (p_branch.game_id <> p_build.game_id) THEN RETURN 'game_mismatch'; END IF;
    IF (p_branch.platform <> p_build.platform) THEN RETURN 'platform_mismatch'; END IF;

    -- Update game branch
    UPDATE game_branches SET build_id = _build_id WHERE id = _branch_id;
    
    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Publish draft build
CREATE OR REPLACE FUNCTION publish_draft_build(_build_id bigint) RETURNS text AS $$
DECLARE
    p_build game_builds;
    p_has_files boolean;
    p_exe_path_valid boolean;
    p_total_size bigint;
    p_compressed_size bigint;
    p_manifest jsonb;
BEGIN
    -- Get game build and check it
    SELECT * INTO p_build FROM game_builds WHERE id = _build_id;
    IF (p_build.id IS NULL OR p_build.is_deleted) THEN RETURN 'build_not_exists'; END IF;
    IF (NOT p_build.is_draft) THEN RETURN 'already_published'; END IF;

    -- Check if draft build contains any files
    SELECT COUNT(*) > 0 INTO p_has_files FROM game_build_files WHERE build_id = _build_id;
    IF (NOT p_has_files) THEN RETURN 'no_files'; END IF;

    -- Check if executable path is empty
    IF (p_build.exe_path = '') THEN RETURN 'invalid_exe_path'; END IF;

    -- Check if executable path is valid
    SELECT COUNT(*) > 0 INTO p_exe_path_valid FROM game_build_files
    WHERE build_id = _build_id AND file_path IN (p_build.exe_path, REGEXP_REPLACE(p_build.exe_path, '\\{1,2}|/', '/'));

    IF (NOT p_exe_path_valid) THEN RETURN 'invalid_exe_path'; END IF;

    -- Get total compressed and uncompressed sizes
    SELECT SUM(file_size)::bigint INTO p_total_size FROM game_build_files WHERE build_id = _build_id;
    SELECT SUM(compressed_file_size)::bigint INTO p_compressed_size FROM game_build_files WHERE build_id = _build_id;

    -- Create manifest
    SELECT jsonb_build_object (
        'files',
        jsonb_agg(
            jsonb_build_object(
                'relative_path', file_path,
                'relative_compressed_path', compressed_file_path,
                'size', file_size,
                'compressed_size', compressed_file_size,
                'md5', md5
            )
        )
    ) INTO p_manifest
    FROM game_build_files
    WHERE build_id = _build_id;

    -- Set build manifest
    INSERT INTO game_manifests (build_id, manifest) VALUES (_build_id, p_manifest);

    -- Delete draft build files
    DELETE FROM game_build_files WHERE build_id = _build_id;

    -- Reset build draft flag
    UPDATE game_builds
    SET
        is_draft = FALSE,
        total_size = p_total_size,
        compressed_size = p_compressed_size,
        updated_at = current_timestamp
    WHERE id = _build_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Deactivate personnel account
CREATE OR REPLACE FUNCTION deactivate_personnel_account(_personnel_id bigint) RETURNS text AS $$
DECLARE
    p_personnel personnel;
BEGIN
    -- Get personnel account and check it
    SELECT * INTO p_personnel FROM personnel WHERE id = _personnel_id;
    IF (p_personnel.id IS NULL) THEN RETURN 'account_not_exists'; END IF;

    -- Delete account sessions
    DELETE FROM personnel_sessions WHERE personnel_id = _personnel_id;

    -- Mark account as deleted
    UPDATE personnel
    SET is_deleted = TRUE, updated_at = current_timestamp
    WHERE id = _personnel_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Set superadmin personnel group
CREATE OR REPLACE FUNCTION set_superadmin_personnel_group(_group_name text) RETURNS text AS $$
BEGIN
    -- Unset previous superadmin personnel groups
    UPDATE personnel_groups
    SET is_superadmin = FALSE, updated_at = current_timestamp
    WHERE LOWER(TRIM(name)) <> LOWER(TRIM(_group_name)) AND is_superadmin;
    
    -- Set current superadmin personnel group
    UPDATE personnel_groups
    SET is_superadmin = TRUE, updated_at = current_timestamp
    WHERE LOWER(TRIM(name)) = LOWER(TRIM(_group_name)) AND NOT is_superadmin;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Check if game is accessible by personnel account
CREATE OR REPLACE FUNCTION is_game_accessible_by_personnel(_game_id text, _personnel_id bigint) RETURNS boolean AS $$
DECLARE
    p_accessible boolean;
BEGIN
    -- Check for personnel account roles
    SELECT COUNT(*) > 0 INTO p_accessible
    FROM personnel_roles
    WHERE personnel_id = _personnel_id AND game_id = _game_id;

    IF (p_accessible) THEN RETURN TRUE; END IF;

    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_accessible
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = pergm.group_id AND pergr.game_id = _game_id)
    WHERE pergm.personnel_id IS NOT NULL AND (perg.is_superadmin OR pergr.role IS NOT NULL);

    RETURN p_accessible;
END;
$$ LANGUAGE plpgsql STABLE;

-- Check if personnel account is allowed to manage at least one game details, branches and builds
CREATE OR REPLACE FUNCTION is_game_manager(_personnel_id bigint) RETURNS boolean AS $$
DECLARE
    p_managable boolean;
BEGIN
    -- Check for personnel account roles
    SELECT COUNT(*) > 0 INTO p_managable
    FROM personnel_roles
    WHERE personnel_id = _personnel_id AND role IN ('maintainer', 'admin');

    IF (p_managable) THEN RETURN TRUE; END IF;

    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_managable
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = pergm.group_id)
    WHERE pergm.personnel_id IS NOT NULL AND (perg.is_superadmin OR pergr.role IN ('maintainer', 'admin'));

    RETURN p_managable;
END;
$$ LANGUAGE plpgsql STABLE;

-- Check if personnel account is allowed to manage game details, branches and builds
CREATE OR REPLACE FUNCTION is_game_manager(_game_id text, _personnel_id bigint) RETURNS boolean AS $$
DECLARE
    p_managable boolean;
BEGIN
    -- Check for personnel account roles
    SELECT COUNT(*) > 0 INTO p_managable
    FROM personnel_roles
    WHERE personnel_id = _personnel_id AND game_id = _game_id AND role IN ('maintainer', 'admin');

    IF (p_managable) THEN RETURN TRUE; END IF;

    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_managable
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = pergm.group_id AND pergr.game_id = _game_id)
    WHERE pergm.personnel_id IS NOT NULL AND (perg.is_superadmin OR pergr.role IN ('maintainer', 'admin'));

    RETURN p_managable;
END;
$$ LANGUAGE plpgsql STABLE;

-- Check if personnel account is superadmin
CREATE OR REPLACE FUNCTION is_superadmin(_personnel_id bigint) RETURNS boolean AS $$
DECLARE
    p_superadmin boolean;
BEGIN
    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_superadmin
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    WHERE pergm.personnel_id IS NOT NULL AND perg.is_superadmin;

    RETURN p_superadmin;
END;
$$ LANGUAGE plpgsql STABLE;

-- Get role numeric representation
CREATE OR REPLACE FUNCTION role_level(_role role_t) RETURNS integer AS $$
BEGIN
    CASE _role
        WHEN 'consumer' THEN RETURN 1;
        WHEN 'uploader' THEN RETURN 2;
        WHEN 'maintainer' THEN RETURN 3;
        WHEN 'admin' THEN RETURN 4;
        ELSE RETURN 0;
    END CASE;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Check if game access role is sufficient
CREATE OR REPLACE FUNCTION is_game_access_role_sufficient(_game_id text, _personnel_id bigint, _min_role role_t) RETURNS boolean AS $$
DECLARE
    p_accessible boolean;
BEGIN
    -- Check for personnel account roles
    SELECT COUNT(*) > 0 INTO p_accessible
    FROM personnel_roles
    WHERE personnel_id = _personnel_id AND game_id = _game_id AND role_level(role) >= role_level(_min_role);

    IF (p_accessible) THEN RETURN TRUE; END IF;

    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_accessible
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = pergm.group_id AND pergr.game_id = _game_id)
    WHERE pergm.personnel_id IS NOT NULL AND (perg.is_superadmin OR role_level(pergr.role) >= role_level(_min_role));

    RETURN p_accessible;
END;
$$ LANGUAGE plpgsql STABLE;

-- Fetch categories assigned to a game
CREATE OR REPLACE FUNCTION assigned_game_categories(_game_id text) RETURNS jsonb AS $$
DECLARE
    p_result jsonb;
BEGIN
    SELECT jsonb_agg(category_id)::jsonb INTO p_result
    FROM game_category_assignments
    WHERE game_id = _game_id
    GROUP BY game_id;

    RETURN COALESCE(p_result, '[]'::jsonb);
END;
$$ LANGUAGE plpgsql STABLE;
