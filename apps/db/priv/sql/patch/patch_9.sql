-- Patch SQL
-- Revision: 8 -> 9

-- Add is_disabled field to games table and related index
ALTER TABLE games ADD COLUMN is_disabled boolean NOT NULL DEFAULT FALSE;
CREATE INDEX game_is_disabled_index ON games (is_disabled);
