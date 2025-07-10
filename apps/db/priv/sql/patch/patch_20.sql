-- Patch SQL
-- Revision: 19 -> 20

-- Add crash_report_path field to game_builds table
ALTER TABLE game_builds ADD COLUMN crash_report_path varchar NOT NULL DEFAULT '';
