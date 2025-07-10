-- Patch SQL
-- Revision: 7 -> 8

-- Add discord_url field to games table
ALTER TABLE games ADD COLUMN discord_url varchar DEFAULT NULL;
