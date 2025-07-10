-- Patch SQL
-- Revision: 37 -> 38

-- Drop is_uploading and add is_processing field to game_builds table
ALTER TABLE game_builds DROP COLUMN is_uploading;
ALTER TABLE game_builds ADD COLUMN is_processing boolean NOT NULL DEFAULT FALSE;
