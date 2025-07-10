-- Patch SQL
-- Revision: 3 -> 4

-- Game manifests
CREATE TABLE game_manifests
(
    build_id bigint PRIMARY KEY REFERENCES game_builds (id),
    manifest jsonb DEFAULT '{}',
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE INDEX gmn_created_at_index ON game_manifests (created_at);
CREATE INDEX gmn_updated_at_index ON game_manifests (updated_at);
