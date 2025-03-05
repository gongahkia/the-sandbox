import 'package:flutter/foundation.dart';
import 'package:supabase_flutter/supabase_flutter.dart';
import '../models/user.dart';

class AuthService extends ChangeNotifier {
  final SupabaseClient _supabase = Supabase.instance.client;
  User? _user;

  User? get user => _user;

  Future<void> signUp(String email, String password) async {
    try {
      // First, sign up with Supabase Auth
      final authResponse = await _supabase.auth.signUp(
        email: email,
        password: password,
      );
      if (authResponse.user == null) throw Exception('Signup failed');

      // Then, get or create user in our custom users table
      final response = await _supabase.rpc('get_or_create_user_id', params: {'p_email': email});
      final userId = response as int;

      // Fetch the user details from the users table
      final userData = await _supabase
          .from('users')
          .select()
          .eq('id', userId)
          .single();
      
      _user = User.fromJson(userData);
      notifyListeners();
    } catch (e) {
      print('Error during signup: $e');
      rethrow;
    }
  }

  Future<void> signIn(String email, String password) async {
    try {
      // First, sign in with Supabase Auth
      final authResponse = await _supabase.auth.signInWithPassword(
        email: email,
        password: password,
      );
      if (authResponse.user == null) throw Exception('Login failed');

      // Then, get user id from our custom users table
      final response = await _supabase.rpc('get_or_create_user_id', params: {'p_email': email});
      final userId = response as int;

      // Fetch the user details from the users table
      final userData = await _supabase
          .from('users')
          .select()
          .eq('id', userId)
          .single();
      
      _user = User.fromJson(userData);
      notifyListeners();
    } catch (e) {
      print('Error during signin: $e');
      rethrow;
    }
  }

  Future<void> signOut() async {
    try {
      await _supabase.auth.signOut();
      _user = null;
      notifyListeners();
    } catch (e) {
      print('Error during signout: $e');
      rethrow;
    }
  }
}