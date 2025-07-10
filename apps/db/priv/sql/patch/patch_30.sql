-- Patch SQL
-- Revision: 29 -> 30

-- Add is_draft field to game_builds table and related index
ALTER TABLE game_builds ADD COLUMN is_draft boolean NOT NULL DEFAULT FALSE;
CREATE INDEX gbl_is_draft_index ON game_builds (is_draft);

-- Game build files (for draft builds)
CREATE TABLE game_build_files
(
    id bigserial PRIMARY KEY,
    build_id bigint REFERENCES game_builds (id),
    file_path varchar NOT NULL,
    file_size bigint NOT NULL CHECK (file_size >= 0),
    compressed_file_path varchar NOT NULL,
    compressed_file_size bigint NOT NULL CHECK (compressed_file_size >= 0),
    md5 varchar NOT NULL,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX gblf_build_file_uc_index ON game_build_files (build_id, file_path);
CREATE INDEX gblf_build_id_index ON game_build_files (build_id);
CREATE INDEX gblf_file_path_index ON game_build_files (file_path);
CREATE INDEX gblf_file_size_index ON game_build_files (file_size);
CREATE INDEX gblf_compressed_file_path_index ON game_build_files (compressed_file_path);
CREATE INDEX gblf_compressed_file_size_index ON game_build_files (compressed_file_size);
CREATE INDEX gblf_created_at_index ON game_build_files (created_at);
CREATE INDEX gblf_updated_at_index ON game_build_files (updated_at);
