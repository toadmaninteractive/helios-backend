-- Patch SQL
-- Revision: 40 -> 41

-- Add registry_config field to game_branches table
ALTER TABLE game_branches ADD COLUMN registry_config jsonb DEFAULT '[]';
