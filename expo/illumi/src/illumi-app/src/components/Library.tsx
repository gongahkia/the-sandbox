import React, { useEffect, useState } from 'react';
import { View, Text, FlatList, Image, StyleSheet } from 'react-native';
import { supabase } from '../lib/supabase';

export default function Library({ userId }) {
  const [books, setBooks] = useState([]);

  useEffect(() => {
    fetchBooks();

    // Set up real-time subscription
    const subscription = supabase
      .from('user_books')
      .on('*', (payload) => {
        console.log('Change received!', payload);
        fetchBooks(); // Refetch books when a change occurs
      })
      .subscribe();

    // Clean up subscription on component unmount
    return () => {
      subscription.unsubscribe();
    };
  }, [userId]);

  async function fetchBooks() {
    if (!userId) return;

    let { data, error } = await supabase
      .from('user_books')
      .select('*')
      .eq('user_id', userId);

    if (error) {
      console.error('Error fetching books:', error);
    } else {
      setBooks(data || []);
    }
  }

  return (
    <View style={styles.container}>
      <Text style={styles.title}>My Library</Text>
      <FlatList
        data={books}
        keyExtractor={(item) => item.id}
        renderItem={({ item }) => (
          <View style={styles.bookItem}>
            {item.cover_url && (
              <Image source={{ uri: item.cover_url }} style={styles.cover} />
            )}
            <View style={styles.bookInfo}>
              <Text style={styles.bookTitle}>{item.title}</Text>
              <Text>{item.author}</Text>
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
  title: {
    fontSize: 24,
    fontWeight: 'bold',
    marginBottom: 20,
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
  bookTitle: {
    fontWeight: 'bold',
  },
});