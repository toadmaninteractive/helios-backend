-- Patch SQL
-- Revision: 21 -> 22

-- Deactivate personnel account
CREATE OR REPLACE FUNCTION deactivate_personnel_account(_personnel_id bigint) RETURNS text AS $$
DECLARE
    p_personnel personnel;
BEGIN
    -- Get personnel account and check it
    SELECT * INTO p_personnel FROM personnel WHERE id = _personnel_id;
    IF (p_personnel.id IS NULL) THEN RETURN 'account_not_exists'; END IF;

    -- Delete account permissions
    DELETE FROM personnel_user_acl WHERE personnel_id = _personnel_id;

    -- Delete account sessions
    DELETE FROM personnel_sessions WHERE personnel_id = _personnel_id;

    -- Mark account as deleted
    UPDATE personnel
    SET is_deleted = TRUE, updated_at = current_timestamp
    WHERE id = _personnel_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;
