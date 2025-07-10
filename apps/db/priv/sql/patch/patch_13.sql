-- Patch SQL
-- Revision: 12 -> 13

-- Add game_engine and is_reportable fields to game_branches table and related indexes
ALTER TABLE game_branches ADD COLUMN game_engine varchar DEFAULT NULL CHECK (TRIM(game_engine) <> '');
ALTER TABLE game_branches ADD COLUMN is_reportable boolean NOT NULL DEFAULT FALSE;
CREATE INDEX gbr_game_engine_index ON game_branches (game_engine);
CREATE INDEX gbr_is_reportable_index ON game_branches (is_reportable);
