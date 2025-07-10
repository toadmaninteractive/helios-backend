-- Patch SQL
-- Revision: 41 -> 42

-- Add optional_file_masks field to game_builds table
ALTER TABLE game_builds ADD COLUMN optional_file_masks jsonb DEFAULT '[]';
