import React, { useState, useEffect } from 'react';
import { View, StyleSheet } from 'react-native';
import Auth from './src/components/Auth';
import BookSearch from './src/components/BookSearch';
import Library from './src/components/Library';

export default function App() {
  const [user, setUser] = useState(null);

  const handleAuthStateChange = (userData) => {
    setUser(userData);
  };

  return (
    <View style={styles.container}>
      {user ? (
        <View style={styles.contentContainer}>
          <BookSearch userId={user.id} />
          <Library userId={user.id} />
        </View>
      ) : (
        <Auth onAuthStateChange={handleAuthStateChange} />
      )}
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#fff',
  },
  contentContainer: {
    flex: 1,
    padding: 20,
  },
});