-- Patch SQL
-- Revision: 22 -> 23

-- Personnel groups
CREATE TABLE personnel_groups
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    name varchar UNIQUE NOT NULL CHECK (TRIM(name) <> ''),
    description varchar DEFAULT NULL,
    is_deleted boolean NOT NULL DEFAULT FALSE,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX perg_name_ult_index ON personnel_groups (LOWER(TRIM(name)));
CREATE INDEX perg_is_deleted_index ON personnel_groups (is_deleted);
CREATE INDEX perg_created_at_index ON personnel_groups (created_at);
CREATE INDEX perg_updated_at_index ON personnel_groups (updated_at);
