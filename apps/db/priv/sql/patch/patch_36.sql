-- Patch SQL
-- Revision: 35 -> 36

-- Get role numeric representation
CREATE OR REPLACE FUNCTION role_level(_role role_t) RETURNS integer AS $$
BEGIN
    CASE _role
        WHEN 'consumer' THEN RETURN 1;
        WHEN 'uploader' THEN RETURN 2;
        WHEN 'maintainer' THEN RETURN 3;
        WHEN 'admin' THEN RETURN 4;
        ELSE RETURN 0;
    END CASE;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Check if game access role is sufficient
CREATE OR REPLACE FUNCTION is_game_access_role_sufficient(_game_id text, _personnel_id bigint, _min_role role_t) RETURNS boolean AS $$
DECLARE
    p_accessible boolean;
BEGIN
    -- Check for personnel account roles
    SELECT COUNT(*) > 0 INTO p_accessible
    FROM personnel_roles
    WHERE personnel_id = _personnel_id AND game_id = _game_id AND role_level(role) >= role_level(_min_role);

    IF (p_accessible) THEN RETURN TRUE; END IF;

    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_accessible
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = pergm.group_id AND pergr.game_id = _game_id)
    WHERE pergm.personnel_id IS NOT NULL AND (perg.is_superadmin OR role_level(pergr.role) >= role_level(_min_role));

    RETURN p_accessible;
END;
$$ LANGUAGE plpgsql STABLE;
