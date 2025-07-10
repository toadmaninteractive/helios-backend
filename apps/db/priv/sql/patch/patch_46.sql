-- Patch SQL
-- Revision: 45 -> 46

-- Add pdb_files field to game_builds table
ALTER TABLE game_builds ADD COLUMN pdb_files jsonb DEFAULT '[]';
