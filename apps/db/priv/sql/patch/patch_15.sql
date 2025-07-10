-- Patch SQL
-- Revision: 14 -> 15

-- Add cleanup_period field to games table and related index
ALTER TABLE games ADD COLUMN cleanup_period integer NOT NULL DEFAULT 0 CHECK (cleanup_period >= 0);
CREATE INDEX game_cleanup_period_index ON games (cleanup_period);
