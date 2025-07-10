-- Patch SQL
-- Revision: 44 -> 45

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

-- Add current assignments
INSERT INTO game_branch_assignments AS gba (branch_id, build_id, assigned_at)
SELECT
    gb.id AS branch_id,
    gb.build_id AS build_id,
    gbl.created_at AS assigned_at
FROM game_branches AS gb
LEFT OUTER JOIN game_builds AS gbl ON (gb.build_id = gbl.id)
WHERE gb.build_id IS NOT NULL
ON CONFLICT (branch_id, build_id) DO UPDATE SET assigned_at = excluded.assigned_at;
