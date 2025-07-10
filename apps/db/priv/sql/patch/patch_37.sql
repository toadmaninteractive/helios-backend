-- Patch SQL
-- Revision: 36 -> 37

-- Add is_uploading, archived_size and processed_size fields to game_builds table
ALTER TABLE game_builds ADD COLUMN is_uploading boolean NOT NULL DEFAULT FALSE;
ALTER TABLE game_builds ADD COLUMN archived_size bigint NOT NULL DEFAULT 0 CHECK (archived_size >= 0);
ALTER TABLE game_builds ADD COLUMN processed_size bigint NOT NULL DEFAULT 0 CHECK (processed_size >= 0);
