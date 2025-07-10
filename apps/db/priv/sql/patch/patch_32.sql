-- Patch SQL
-- Revision: 31 -> 32

-- Publish draft build
CREATE OR REPLACE FUNCTION publish_draft_build(_build_id bigint) RETURNS text AS $$
DECLARE
    p_build game_builds;
    p_has_files boolean;
    p_exe_path_valid boolean;
    p_total_size bigint;
    p_compressed_size bigint;
    p_manifest jsonb;
BEGIN
    -- Get game build and check it
    SELECT * INTO p_build FROM game_builds WHERE id = _build_id;
    IF (p_build.id IS NULL OR p_build.is_deleted) THEN RETURN 'build_not_exists'; END IF;
    IF (NOT p_build.is_draft) THEN RETURN 'already_published'; END IF;

    -- Check if draft build contains any files
    SELECT COUNT(*) > 0 INTO p_has_files FROM game_build_files WHERE build_id = _build_id;
    IF (NOT p_has_files) THEN RETURN 'no_files'; END IF;

    -- Check if executable path is empty
    IF (p_build.exe_path = '') THEN RETURN 'invalid_exe_path'; END IF;

    -- Check if executable path is valid
    SELECT COUNT(*) > 0 INTO p_exe_path_valid FROM game_build_files
    WHERE build_id = 51 AND file_path IN (p_build.exe_path, REGEXP_REPLACE(p_build.exe_path, '\\{1,2}|/', '/'));

    IF (NOT p_exe_path_valid) THEN RETURN 'invalid_exe_path'; END IF;

    -- Get total compressed and uncompressed sizes
    SELECT SUM(file_size)::bigint INTO p_total_size FROM game_build_files WHERE build_id = _build_id;
    SELECT SUM(compressed_file_size)::bigint INTO p_compressed_size FROM game_build_files WHERE build_id = _build_id;

    -- Create manifest
    SELECT jsonb_build_object (
        'files',
        jsonb_agg(
            jsonb_build_object(
                'relative_path', file_path,
                'relative_compressed_path', compressed_file_path,
                'size', file_size,
                'compressed_size', compressed_file_size,
                'md5', md5
            )
        )
    ) INTO p_manifest
    FROM game_build_files
    WHERE build_id = _build_id;

    -- Set build manifest
    INSERT INTO game_manifests (build_id, manifest) VALUES (_build_id, p_manifest);

    -- Delete draft build files
    DELETE FROM game_build_files WHERE build_id = _build_id;

    -- Reset build draft flag
    UPDATE game_builds
    SET
        is_draft = FALSE,
        total_size = p_total_size,
        compressed_size = p_compressed_size,
        updated_at = current_timestamp
    WHERE id = _build_id;

    RETURN 'ok';
END;
$$ LANGUAGE plpgsql VOLATILE;
