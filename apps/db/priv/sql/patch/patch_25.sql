-- Patch SQL
-- Revision: 24 -> 25

-- Create role_t enum type
CREATE TYPE role_t AS ENUM (
    'consumer',
    'uploader',
    'maintainer',
    'admin'
);

-- Personnel roles
CREATE TABLE personnel_roles
(
    personnel_id bigint NOT NULL REFERENCES personnel (id),
    game_id varchar NOT NULL REFERENCES games (id),
    role role_t NOT NULL,
    PRIMARY KEY (personnel_id, game_id)
) WITHOUT OIDS;

CREATE INDEX perr_personnel_id_index ON personnel_roles (personnel_id);
CREATE INDEX perr_game_id_index ON personnel_roles (game_id);
CREATE INDEX perr_role_index ON personnel_roles (role);

-- Personnel group roles
CREATE TABLE personnel_group_roles
(
    group_id bigint NOT NULL REFERENCES personnel_groups (id),
    game_id varchar NOT NULL REFERENCES games (id),
    role role_t NOT NULL,
    PRIMARY KEY (group_id, game_id)
) WITHOUT OIDS;

CREATE INDEX pergr_group_id_index ON personnel_group_roles (group_id);
CREATE INDEX pergr_game_id_index ON personnel_group_roles (game_id);
CREATE INDEX pergr_role_index ON personnel_group_roles (role);
