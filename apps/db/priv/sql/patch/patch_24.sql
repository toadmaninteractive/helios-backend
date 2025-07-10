-- Patch SQL
-- Revision: 23 -> 24

-- Personnel group membership
CREATE TABLE personnel_group_membership
(
    personnel_id bigint NOT NULL REFERENCES personnel (id),
    group_id bigint NOT NULL REFERENCES personnel_groups (id),
    PRIMARY KEY (personnel_id, group_id)
) WITHOUT OIDS;

CREATE INDEX pergm_personnel_id_index ON personnel_group_membership (personnel_id);
CREATE INDEX pergm_group_id_index ON personnel_group_membership (group_id);