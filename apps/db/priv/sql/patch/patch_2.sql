-- Patch SQL
-- Revision: 1 -> 2

-- Custom types
CREATE TYPE ownership_t AS ENUM (
    'none',
    'purchase',
    'grant',
    'employee'
);

-- Games
CREATE TABLE games
(
    id varchar PRIMARY KEY CHECK (TRIM(id) <> ''),
    title varchar NOT NULL CHECK (TRIM(title) <> ''),
    description varchar NOT NULL DEFAULT '',
    jira_key varchar DEFAULT NULL,
    price numeric(15, 6) NOT NULL DEFAULT 0 CHECK (price >= 0),
    currency varchar NOT NULL,
    is_published boolean NOT NULL DEFAULT FALSE,
    is_deleted boolean NOT NULL DEFAULT FALSE,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX game_title_ult_index ON games (LOWER(TRIM(title)));
CREATE INDEX game_jira_key_ut_index ON games (UPPER(TRIM(jira_key)));
CREATE INDEX game_is_published_index ON games (is_published);
CREATE INDEX game_is_deleted_index ON games (is_deleted);
CREATE INDEX game_created_at_index ON games (created_at);
CREATE INDEX game_updated_at_index ON games (updated_at);

-- Game builds
CREATE TABLE game_builds
(
    id bigserial PRIMARY KEY,
    game_id varchar NOT NULL REFERENCES games (id),
    build_rev varchar NOT NULL CHECK (TRIM(build_rev) <> ''),
    commentary varchar NOT NULL DEFAULT '',
    change_list varchar NOT NULL DEFAULT '',
    total_size bigint NOT NULL CHECK (total_size >= 0),
    compressed_size bigint NOT NULL CHECK (compressed_size >= 0),
    exe_path varchar NOT NULL DEFAULT '',
    log_path varchar NOT NULL DEFAULT '',
    cdn_root_url varchar NOT NULL CHECK (TRIM(cdn_root_url) <> ''),
    is_deleted boolean NOT NULL DEFAULT FALSE,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX gbl_game_build_uc_index ON game_builds (game_id, build_rev);
CREATE INDEX gbl_game_id_index ON game_builds (game_id);
CREATE INDEX gbl_total_size_index ON game_builds (total_size);
CREATE INDEX gbl_compressed_size_index ON game_builds (compressed_size);
CREATE INDEX gbl_is_deleted_index ON game_builds (is_deleted);
CREATE INDEX gbl_created_at_index ON game_builds (created_at);
CREATE INDEX gbl_updated_at_index ON game_builds (updated_at);

-- Game branches
CREATE TABLE game_branches
(
    id bigserial PRIMARY KEY,
    game_id varchar NOT NULL REFERENCES games (id),
    build_id bigint DEFAULT NULL REFERENCES game_builds (id),
    title varchar NOT NULL CHECK (TRIM(title) <> ''),
    description varchar NOT NULL DEFAULT '',
    password varchar NOT NULL DEFAULT '',
    is_public boolean NOT NULL DEFAULT FALSE,
    is_default boolean NOT NULL DEFAULT FALSE,
    is_deleted boolean NOT NULL DEFAULT FALSE,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX gbr_game_branch_uc_index ON game_branches (game_id, LOWER(TRIM(title)));
CREATE INDEX gbr_game_id_index ON game_branches (game_id);
CREATE INDEX gbr_build_id_index ON game_branches (build_id);
CREATE INDEX gbr_is_public_index ON game_branches (is_public);
CREATE INDEX gbr_is_default_index ON game_branches (is_default);
CREATE INDEX gbr_is_deleted_index ON game_branches (is_deleted);
CREATE INDEX gbr_created_at_index ON game_branches (created_at);
CREATE INDEX gbr_updated_at_index ON game_branches (updated_at);

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
