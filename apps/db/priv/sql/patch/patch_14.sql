-- Patch SQL
-- Revision: 13 -> 14

-- Add is_permanent field to game_builds table and related index
ALTER TABLE game_builds ADD COLUMN is_permanent boolean NOT NULL DEFAULT FALSE;
CREATE INDEX gbl_is_permanent_index ON game_builds (is_permanent);
