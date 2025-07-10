-- Patch SQL
-- Revision: 20 -> 21

-- Add name field to personnel table
ALTER TABLE personnel ADD COLUMN name varchar DEFAULT NULL;
