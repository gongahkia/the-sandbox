import 'dart:convert';
import 'package:http/http.dart' as http;
import 'package:supabase_flutter/supabase_flutter.dart';
import '../models/user.dart' as app_user;

class BookService {
  final SupabaseClient _supabase = Supabase.instance.client;

  Future<List<Book>> searchBooks(String query) async {
    try {
      final response = await http.get(
        Uri.parse('https://openlibrary.org/search.json?q=$query'),
      );

      if (response.statusCode == 200) {
        final data = json.decode(response.body);
        return (data['docs'] as List)
            .map((book) => Book.fromOpenLibrary(book))
            .toList();
      } else {
        throw Exception('Failed to load books');
      }
    } catch (e) {
      print('Error searching books: $e');
      rethrow;
    }
  }

  Future<void> addFavorite(app_user.User user, Book book) async {
    try {
      await _supabase.from('favorites').insert({
        'user_id': user.id,
        'book_key': book.key,
        'title': book.title,
        'author': book.author,
      });
    } catch (e) {
      print('Error adding favorite: $e');
      rethrow;
    }
  }

  Future<List<Book>> getFavorites(app_user.User user) async {
    try {
      final response = await _supabase
          .from('favorites')
          .select()
          .eq('user_id', user.id);
      return response.map((book) => Book.fromJson(book)).toList();
    } catch (e) {
      print('Error getting favorites: $e');
      rethrow;
    }
  }
}

class Book {
  final String key;
  final String title;
  final String author;

  Book({required this.key, required this.title, required this.author});

  factory Book.fromOpenLibrary(Map<String, dynamic> json) {
    return Book(
      key: json['key'],
      title: json['title'],
      author: json['author_name']?.first ?? 'Unknown',
    );
  }

  factory Book.fromJson(Map<String, dynamic> json) {
    return Book(
      key: json['book_key'],
      title: json['title'],
      author: json['author'],
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'book_key': key,
      'title': title,
      'author': author,
    };
  }
}