-- Patch SQL
-- Revision: 9 -> 10

-- Add ci_url field to games table
ALTER TABLE games ADD COLUMN ci_url varchar DEFAULT NULL;
