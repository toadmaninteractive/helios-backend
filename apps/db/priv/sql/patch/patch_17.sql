-- Patch SQL
-- Revision: 16 -> 17

-- Add platform_t enum type
CREATE TYPE platform_t AS ENUM (
    'windows',
    'linux',
    'macos',
    'ios',
    'android'
);

-- Add platform field to game_builds table and related index
ALTER TABLE game_builds ADD COLUMN platform platform_t NOT NULL DEFAULT 'windows';
CREATE INDEX gbl_platform_index ON game_builds (platform);

-- Add platform field to game_branches table and related index
ALTER TABLE game_branches ADD COLUMN platform platform_t NOT NULL DEFAULT 'windows';
CREATE INDEX gbr_platform_index ON game_branches (platform);
CREATE UNIQUE INDEX gbr_platform_default_index ON game_branches (game_id, platform, is_default) WHERE is_default;
