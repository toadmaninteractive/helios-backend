-- Patch SQL
-- Revision: 11 -> 12

-- Personnel user ACL
CREATE TABLE personnel_user_acl
(
    personnel_id bigint NOT NULL REFERENCES personnel (id),
    game_id varchar NOT NULL REFERENCES games (id),
    is_project_wide boolean NOT NULL DEFAULT FALSE,
    branch_ids jsonb DEFAULT '[]',
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp,
    PRIMARY KEY (personnel_id, game_id)
) WITHOUT OIDS;

CREATE INDEX puacl_personnel_id_index ON personnel_user_acl (personnel_id);
CREATE INDEX puacl_game_id_index ON personnel_user_acl (game_id);
CREATE INDEX puacl_is_project_wide_index ON personnel_user_acl (is_project_wide);
CREATE INDEX puacl_branch_ids_gin_index ON personnel_user_acl USING gin (branch_ids);
CREATE INDEX puacl_created_at_index ON personnel_user_acl (created_at);
CREATE INDEX puacl_updated_at_index ON personnel_user_acl (updated_at);
