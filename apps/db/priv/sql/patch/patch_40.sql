-- Patch SQL
-- Revision: 39 -> 40

-- Add api_key field to personnel table and related index
ALTER TABLE personnel ADD COLUMN api_key varchar DEFAULT NULL;
CREATE UNIQUE INDEX per_api_key_ut_index ON personnel (TRIM(api_key)) WHERE api_key IS NOT NULL;
