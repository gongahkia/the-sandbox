CREATE TABLE passwords (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
    password TEXT NOT NULL,
    description TEXT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);
COMMENT ON TABLE passwords IS 'Stores generated passwords and their descriptions';
COMMENT ON COLUMN passwords.id IS 'Unique identifier for each password entry';
COMMENT ON COLUMN passwords.password IS 'The generated password';
COMMENT ON COLUMN passwords.description IS 'User-provided description for the password';
COMMENT ON COLUMN passwords.created_at IS 'Timestamp when the password was created';
CREATE INDEX idx_passwords_description ON passwords (description);
ALTER TABLE passwords ENABLE ROW LEVEL SECURITY;
CREATE POLICY "Allow all operations" ON passwords FOR ALL USING (true);