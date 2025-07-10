-- Patch SQL
-- Revision: 25 -> 26

-- Add is_superadmin field to personnel_groups table and related index
ALTER TABLE personnel_groups ADD COLUMN is_superadmin boolean NOT NULL DEFAULT FALSE;
CREATE INDEX perg_is_superadmin_index ON personnel_groups (is_superadmin);

-- Set superadmin personnel group
CREATE OR REPLACE FUNCTION set_superadmin_personnel_group(_group_name text) RETURNS text AS $$
BEGIN
    -- Unset previous superadmin personnel groups
    UPDATE personnel_groups
    SET is_superadmin = FALSE, updated_at = current_timestamp
    WHERE LOWER(TRIM(name)) <> LOWER(TRIM(_group_name)) AND is_superadmin;
    
    -- Set current superadmin personnel group
    UPDATE personnel_groups
    SET is_superadmin = TRUE, updated_at = current_timestamp
    WHERE LOWER(TRIM(name)) = LOWER(TRIM(_group_name)) AND NOT is_superadmin;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;
