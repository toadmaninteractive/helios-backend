-- Patch SQL
-- Revision: 32 -> 33

-- Set build_id field from game_build_files table as not null
ALTER TABLE game_build_files ALTER COLUMN build_id SET NOT NULL;
