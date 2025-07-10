-- Patch SQL
-- Revision: 15 -> 16

-- Delete cleanup_period field and related index
DROP INDEX IF EXISTS game_cleanup_period_index;
ALTER TABLE games DROP COLUMN cleanup_period;

-- Add build_lifetime field to games table and related index
ALTER TABLE games ADD COLUMN build_lifetime integer NOT NULL DEFAULT 0 CHECK (build_lifetime >= 0);
CREATE INDEX game_build_lifetime_index ON games (build_lifetime);
