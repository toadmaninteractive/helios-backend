-- Patch SQL
-- Revision: 34 -> 35

-- Add is_global and branch_ids fields to personnel_roles table and related indexes
ALTER TABLE personnel_roles ADD COLUMN is_global boolean NOT NULL DEFAULT TRUE;
ALTER TABLE personnel_roles ADD COLUMN branch_ids jsonb NOT NULL DEFAULT '[]';

CREATE INDEX perr_is_global_index ON personnel_roles (is_global);
CREATE INDEX perr_branch_ids_gin_index ON personnel_roles USING gin (branch_ids);

-- Add is_global and branch_ids fields to personnel_group_roles table and related indexes
ALTER TABLE personnel_group_roles ADD COLUMN is_global boolean NOT NULL DEFAULT TRUE;
ALTER TABLE personnel_group_roles ADD COLUMN branch_ids jsonb NOT NULL DEFAULT '[]';

CREATE INDEX pergr_is_global_index ON personnel_group_roles (is_global);
CREATE INDEX pergr_branch_ids_gin_index ON personnel_group_roles USING gin (branch_ids);
