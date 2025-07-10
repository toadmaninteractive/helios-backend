-- Patch SQL
-- Revision: 48 -> 49

-- Fetch categories assigned to a game
CREATE OR REPLACE FUNCTION assigned_game_categories(_game_id text) RETURNS jsonb AS $$
DECLARE
    p_result jsonb;
BEGIN
    SELECT jsonb_agg(category_id)::jsonb INTO p_result
    FROM game_category_assignments
    WHERE game_id = _game_id
    GROUP BY game_id;

    RETURN COALESCE(p_result, '[]'::jsonb);
END;
$$ LANGUAGE plpgsql STABLE;
