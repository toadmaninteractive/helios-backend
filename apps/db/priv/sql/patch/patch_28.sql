-- Patch SQL
-- Revision: 27 -> 28

-- Deactivate personnel account
CREATE OR REPLACE FUNCTION deactivate_personnel_account(_personnel_id bigint) RETURNS text AS $$
DECLARE
    p_personnel personnel;
BEGIN
    -- Get personnel account and check it
    SELECT * INTO p_personnel FROM personnel WHERE id = _personnel_id;
    IF (p_personnel.id IS NULL) THEN RETURN 'account_not_exists'; END IF;

    -- Delete account sessions
    DELETE FROM personnel_sessions WHERE personnel_id = _personnel_id;

    -- Mark account as deleted
    UPDATE personnel
    SET is_deleted = TRUE, updated_at = current_timestamp
    WHERE id = _personnel_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Check if personnel account is allowed to manage game details, branches and builds
CREATE OR REPLACE FUNCTION is_game_manager(_game_id text, _personnel_id bigint) RETURNS boolean AS $$
DECLARE
    p_managable boolean;
BEGIN
    -- Check for personnel account roles
    SELECT COUNT(*) > 0 INTO p_managable
    FROM personnel_roles
    WHERE personnel_id = _personnel_id AND game_id = _game_id AND role IN ('maintainer', 'admin');

    IF (p_managable) THEN RETURN TRUE; END IF;

    -- Check for personnel group roles
    SELECT COUNT(*) > 0 INTO p_managable
    FROM personnel_groups AS perg
    LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id AND pergm.personnel_id = _personnel_id)
    LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = pergm.group_id AND pergr.game_id = _game_id)
    WHERE pergm.personnel_id IS NOT NULL AND (perg.is_superadmin OR pergr.role IN ('maintainer', 'admin'));

    RETURN p_managable;
END;
$$ LANGUAGE plpgsql STABLE;

-- Drop personnel_user_acl table
DROP TABLE IF EXISTS personnel_user_acl;
