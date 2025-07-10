-- Patch SQL
-- Revision: 10 -> 11

-- Add is_local field to game_builds table and related index
ALTER TABLE game_builds ADD COLUMN is_local boolean NOT NULL DEFAULT TRUE;
CREATE INDEX gbl_is_local_index ON game_builds (is_local);
