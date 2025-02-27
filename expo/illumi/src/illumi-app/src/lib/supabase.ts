import 'react-native-url-polyfill/auto';
import AsyncStorage from '@react-native-async-storage/async-storage';
import { createClient } from '@supabase/supabase-js';

const supabaseUrl = "https://ygizfvhkaqjxklvuofzf.supabase.co";
const supabaseAnonKey = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InlnaXpmdmhrYXFqeGtsdnVvZnpmIiwicm9sZSI6ImFub24iLCJpYXQiOjE3NDA2NDM1MTMsImV4cCI6MjA1NjIxOTUxM30.K0ewMPABPClJfwWIHk05Dp-x_hUAkDOZH9WKcDCxSlg";

export const supabase = createClient(supabaseUrl, supabaseAnonKey, {
  auth: {
    storage: AsyncStorage,
    autoRefreshToken: true,
    persistSession: true,
    detectSessionInUrl: false,
  },
});