-- Patch SQL
-- Revision: 28 -> 29

-- Check if personnel account is allowed to manage at least one game details, branches and builds
CREATE OR REPLACE FUNCTION is_game_manager(_personnel_id bigint) RETURNS boolean AS $$
DECLARE
    p_managable boolean;
BEGIN
    -- Check for personnel account roles
    SELECT COUNT(*) > 0 INTO p_managable
    FROM personnel_roles
    WHERE personnel_id = _personnel_id AND role IN ('maintainer', 'admin');

    IF (p_managable) THEN RETURN TRUE; END IF;

    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_managable
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = pergm.group_id)
    WHERE pergm.personnel_id IS NOT NULL AND (perg.is_superadmin OR pergr.role IN ('maintainer', 'admin'));

    RETURN p_managable;
END;
$$ LANGUAGE plpgsql STABLE;

-- Check if personnel account is superadmin
CREATE OR REPLACE FUNCTION is_superadmin(_personnel_id bigint) RETURNS boolean AS $$
DECLARE
    p_superadmin boolean;
BEGIN
    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_superadmin
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    WHERE pergm.personnel_id IS NOT NULL AND perg.is_superadmin;

    RETURN p_superadmin;
END;
$$ LANGUAGE plpgsql STABLE;
