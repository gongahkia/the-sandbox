import React, { useState } from 'react';
import { View, TextInput, Button, FlatList, Text, Image, StyleSheet } from 'react-native';
import axios from 'axios';
import { supabase } from '../lib/supabase';

export default function BookSearch({ userId }) {
  const [query, setQuery] = useState('');
  const [books, setBooks] = useState([]);

  const searchBooks = async () => {
    try {
      const response = await axios.get(`https://openlibrary.org/search.json?q=${query}`);
      setBooks(response.data.docs.slice(0, 10));
    } catch (error) {
      console.error('Error searching books:', error);
    }
  };

  const addBookToLibrary = async (book) => {
    if (!userId) return;

    const { data, error } = await supabase
      .from('user_books')
      .insert({
        user_id: userId,
        book_id: book.key.split('/')[2],
        title: book.title,
        author: book.author_name ? book.author_name[0] : 'Unknown',
        cover_url: book.cover_i ? `https://covers.openlibrary.org/b/id/${book.cover_i}-M.jpg` : null,
      });

    if (error) console.error('Error adding book:', error);
    else console.log('Book added successfully');
  };

  return (
    <View style={styles.container}>
      <TextInput
        style={styles.input}
        value={query}
        onChangeText={setQuery}
        placeholder="Search for books"
      />
      <Button title="Search" onPress={searchBooks} />
      <FlatList
        data={books}
        keyExtractor={(item) => item.key}
        renderItem={({ item }) => (
          <View style={styles.bookItem}>
            {item.cover_i && (
              <Image
                source={{ uri: `https://covers.openlibrary.org/b/id/${item.cover_i}-S.jpg` }}
                style={styles.cover}
              />
            )}
            <View style={styles.bookInfo}>
              <Text style={styles.title}>{item.title}</Text>
              <Text>{item.author_name ? item.author_name[0] : 'Unknown'}</Text>
              <Button title="Add to Library" onPress={() => addBookToLibrary(item)} />
            </View>
          </View>
        )}
      />
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    padding: 10,
  },
  input: {
    height: 40,
    borderColor: 'gray',
    borderWidth: 1,
    marginBottom: 10,
    paddingHorizontal: 10,
  },
  bookItem: {
    flexDirection: 'row',
    marginBottom: 10,
  },
  cover: {
    width: 50,
    height: 75,
    marginRight: 10,
  },
  bookInfo: {
    flex: 1,
  },
  title: {
    fontWeight: 'bold',
  },
});