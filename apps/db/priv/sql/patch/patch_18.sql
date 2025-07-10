-- Patch SQL
-- Revision: 17 -> 18

-- Set default branch
CREATE OR REPLACE FUNCTION set_default_branch(_branch_id bigint) RETURNS text AS $$
DECLARE
    p_branch game_branches;
BEGIN
    -- Get game branch and check it
    SELECT * INTO p_branch FROM game_branches WHERE id = _branch_id;
    IF (p_branch.id IS NULL OR p_branch.is_deleted) THEN RETURN 'branch_not_exists'; END IF;

    -- Reset default branches for given game and platform
    UPDATE game_branches SET is_default = FALSE WHERE game_id = p_branch.game_id AND platform = p_branch.platform;

    -- Set default branch
    UPDATE game_branches SET is_default = TRUE WHERE id = _branch_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Assign build to game branch
CREATE OR REPLACE FUNCTION assign_build_to_branch(_branch_id bigint, _build_id bigint) RETURNS text AS $$
DECLARE
    p_branch game_branches;
    p_build game_builds;
BEGIN
    -- Get game branch and check it
    SELECT * INTO p_branch FROM game_branches WHERE id = _branch_id;
    IF (p_branch.id IS NULL OR p_branch.is_deleted) THEN RETURN 'branch_not_exists'; END IF;

    -- Get game build and check it
    SELECT * INTO p_build FROM game_builds WHERE id = _build_id;
    IF (p_build.id IS NULL OR p_build.is_deleted) THEN RETURN 'build_not_exists'; END IF;

    -- Both game_id and platform should match
    IF (p_branch.game_id <> p_build.game_id) THEN RETURN 'game_mismatch'; END IF;
    IF (p_branch.platform <> p_build.platform) THEN RETURN 'platform_mismatch'; END IF;

    -- Update game branch
    UPDATE game_branches SET build_id = _build_id WHERE id = _branch_id;
    
    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;
