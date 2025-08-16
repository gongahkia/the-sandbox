import { createClient } from '@supabase/supabase-js';

const EXPO_PUBLIC_SUPABASE_URL= "XXX"
const EXPO_PUBLIC_SUPABASE_ANON_KEY= "XXX"

export const supabase = createClient(
  EXPO_PUBLIC_SUPABASE_URL,
  EXPO_PUBLIC_SUPABASE_ANON_KEY
);