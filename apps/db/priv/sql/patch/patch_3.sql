-- Patch SQL
-- Revision: 2 -> 3

-- Add rev field to games, game_builds and game_branches tables
ALTER TABLE games ADD COLUMN rev integer NOT NULL DEFAULT 1;
ALTER TABLE game_builds ADD COLUMN rev integer NOT NULL DEFAULT 1;
ALTER TABLE game_branches ADD COLUMN rev integer NOT NULL DEFAULT 1;