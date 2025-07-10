-- Patch SQL
-- Revision: 38 -> 39

-- Add config_path field to game_builds table
ALTER TABLE game_builds ADD COLUMN config_path varchar NOT NULL DEFAULT '';

-- Add ini_config field to game_branches table
ALTER TABLE game_branches ADD COLUMN ini_config jsonb DEFAULT '[]';
