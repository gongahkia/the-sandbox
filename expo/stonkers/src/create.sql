CREATE OR REPLACE FUNCTION auth.user_id() RETURNS TEXT AS $$
  SELECT nullif(current_setting('request.jwt.claims', true)::json->>'sub', '')::TEXT;
$$ LANGUAGE SQL STABLE;

CREATE TABLE public.steps (
    id BIGSERIAL PRIMARY KEY,
    user_id TEXT NOT NULL,
    date DATE NOT NULL,
    count INTEGER NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_steps_user_id_date ON public.steps(user_id, date);

ALTER TABLE public.steps ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Users can insert their own steps" ON public.steps
    FOR INSERT TO authenticated
    WITH CHECK (auth.user_id() = user_id);

CREATE POLICY "Users can view their own steps" ON public.steps
    FOR SELECT TO authenticated
    USING (auth.user_id() = user_id);

CREATE POLICY "Users can update their own steps" ON public.steps
    FOR UPDATE TO authenticated
    USING (auth.user_id() = user_id);

CREATE POLICY "Users can delete their own steps" ON public.steps
    FOR DELETE TO authenticated
    USING (auth.user_id() = user_id);