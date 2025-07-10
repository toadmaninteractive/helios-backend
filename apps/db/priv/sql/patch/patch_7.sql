-- Patch SQL
-- Revision: 6 -> 7

-- Add selene_key field to games table and create related index
ALTER TABLE games ADD COLUMN selene_key varchar DEFAULT NULL;
CREATE INDEX game_selene_key_ut_index ON games (UPPER(TRIM(selene_key)));
