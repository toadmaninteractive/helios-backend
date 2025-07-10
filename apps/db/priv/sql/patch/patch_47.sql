-- Patch SQL
-- Revision: 46 -> 47

-- Game categories
CREATE TABLE game_categories
(
    id bigserial PRIMARY KEY,
    rev integer NOT NULL DEFAULT 1,
    name varchar NOT NULL CHECK (TRIM(name) <> ''),
    description varchar NOT NULL DEFAULT '',
    sort_order integer NOT NULL DEFAULT 1,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
) WITHOUT OIDS;

CREATE UNIQUE INDEX gc_name_ult_index ON game_categories (LOWER(TRIM(name)));

-- Game category assignments
CREATE TABLE game_category_assignments
(
    game_id varchar NOT NULL REFERENCES games (id),
    category_id bigint NOT NULL REFERENCES game_categories (id),
    assigned_at timestamptz NOT NULL DEFAULT current_timestamp,
    PRIMARY KEY (game_id, category_id)
) WITHOUT OIDS;

CREATE INDEX gca_game_id_index ON game_category_assignments (game_id);
CREATE INDEX gca_category_id_index ON game_category_assignments (category_id);
