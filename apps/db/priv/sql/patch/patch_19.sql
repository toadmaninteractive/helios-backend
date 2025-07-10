-- Patch SQL
-- Revision: 18 -> 19

-- Add new settings
INSERT INTO settings ("param", "type", "value") VALUES
    ('ci_api_key', 'string', NULL);
