-- Patch SQL
-- Revision: 4 -> 5

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
