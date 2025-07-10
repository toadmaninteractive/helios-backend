-- Patch SQL
-- Revision: 26 -> 27

-- Check if game is accessible by personnel account
CREATE OR REPLACE FUNCTION is_game_accessible_by_personnel(_game_id text, _personnel_id bigint) RETURNS boolean AS $$
DECLARE
    p_accessible boolean;
BEGIN
    -- Check for personnel account roles
    SELECT COUNT(*) > 0 INTO p_accessible
    FROM personnel_roles
    WHERE personnel_id = _personnel_id AND game_id = _game_id;

    IF (p_accessible) THEN RETURN TRUE; END IF;

    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_accessible
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = pergm.group_id AND pergr.game_id = _game_id)
    WHERE pergm.personnel_id IS NOT NULL AND (perg.is_superadmin OR pergr.role IS NOT NULL);

    RETURN p_accessible;
END;
$$ LANGUAGE plpgsql STABLE;
