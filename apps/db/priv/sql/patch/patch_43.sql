-- Patch SQL
-- Revision: 42 -> 43

-- Personnel favourite games
CREATE TABLE personnel_favourite_games
(
    personnel_id bigint NOT NULL REFERENCES personnel (id) ON DELETE CASCADE,
    game_id varchar NOT NULL REFERENCES games (id) ON DELETE CASCADE,
    PRIMARY KEY (personnel_id, game_id)
) WITHOUT OIDS;

CREATE INDEX perfg_personnel_id_index ON personnel_favourite_games (personnel_id);
CREATE INDEX perfg_game_id_index ON personnel_favourite_games (game_id);
