CREATE TABLE users (
    id VARCHAR(255) PRIMARY KEY,
    payment_amount NUMERIC(19,4)
);

GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE public.users TO anon, authenticated, service_role;