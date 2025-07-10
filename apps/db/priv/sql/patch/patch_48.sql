-- Patch SQL
-- Revision: 47 -> 48

-- Add ON DELETE CASCADE constraint to category_id
ALTER TABLE game_category_assignments
DROP CONSTRAINT game_category_assignments_category_id_fkey,
ADD CONSTRAINT game_category_assignments_category_id_fkey
    FOREIGN KEY (category_id)
    REFERENCES game_categories (id)
    ON DELETE CASCADE;
