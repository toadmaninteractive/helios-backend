-- Patch SQL
-- Revision: 5 -> 6

-- Delete settings
DELETE FROM settings WHERE param = 'admin_session_duration';

-- Drop tables
DROP TABLE logs;
DROP TABLE admin_sessions;
DROP TABLE admins;

-- Drop enums
DROP TYPE actor_t;
DROP TYPE entity_t;

-- Re-create enums
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

-- Add new settings
INSERT INTO settings ("param", "type", "value") VALUES
    ('personnel_session_duration', 'int', '2592000');

-- Re-create logs table and related indexes
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

-- Create personnel table and related indexes (instead of admins)
CREATE TABLE personnel
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    username varchar UNIQUE NOT NULL CHECK (TRIM(username) <> ''),
    email varchar DEFAULT NULL CHECK (TRIM(email) <> ''),
    phone varchar DEFAULT NULL CHECK (TRIM(phone) <> ''),
    is_blocked boolean NOT NULL DEFAULT FALSE,
    is_deleted boolean NOT NULL DEFAULT FALSE,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX per_username_ult_index ON personnel (LOWER(TRIM(username)));
CREATE UNIQUE INDEX per_email_ult_index ON personnel (LOWER(TRIM(email)));
CREATE UNIQUE INDEX per_phone_ult_index ON personnel (LOWER(TRIM(phone)));
CREATE INDEX per_is_blocked_index ON personnel (is_blocked);
CREATE INDEX per_is_deleted_index ON personnel (is_deleted);
CREATE INDEX per_created_at_index ON personnel (created_at);
CREATE INDEX per_updated_at_index ON personnel (updated_at);

-- Create personnel_sessions table and related indexes (instead of admin_sessions)
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
