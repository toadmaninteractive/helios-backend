-- Patch SQL
-- Revision: 43 -> 44

-- Add preserved_file_masks and redistributables fields to game_builds table
ALTER TABLE game_builds ADD COLUMN preserved_file_masks jsonb DEFAULT '[]';
ALTER TABLE game_builds ADD COLUMN redistributables jsonb DEFAULT '[]';
