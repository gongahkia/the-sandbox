CREATE TABLE users (
    id BIGSERIAL PRIMARY KEY,
    email TEXT UNIQUE NOT NULL,
    password TEXT NOT NULL,
    auth_uid UUID UNIQUE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE favorites (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT REFERENCES users(id) ON DELETE CASCADE,
    book_key TEXT NOT NULL,
    title TEXT NOT NULL,
    author TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_favorites_user_id ON favorites(user_id);

ALTER TABLE users ENABLE ROW LEVEL SECURITY;
ALTER TABLE favorites ENABLE ROW LEVEL SECURITY;

CREATE POLICY users_policy ON users
    FOR ALL
    USING (auth.uid() = auth_uid);

CREATE POLICY favorites_policy ON favorites
    FOR ALL
    USING (auth.uid() = (SELECT auth_uid FROM users WHERE id = favorites.user_id));

CREATE OR REPLACE FUNCTION get_or_create_user_id(p_email TEXT, p_password TEXT)
RETURNS BIGINT AS $$
DECLARE
    v_user_id BIGINT;
BEGIN
    SELECT id INTO v_user_id FROM users WHERE email = p_email;
    
    IF v_user_id IS NULL THEN
        INSERT INTO users (email, password, auth_uid) 
        VALUES (p_email, p_password, auth.uid())
        RETURNING id INTO v_user_id;
    END IF;
    
    RETURN v_user_id;
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;