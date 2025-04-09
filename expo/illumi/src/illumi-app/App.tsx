import React, { useState } from 'react';
import { View, StyleSheet, Button } from 'react-native';
import { supabase } from './src/lib/supabase';
import Auth from './src/components/Auth';
import BookSearch from './src/components/BookSearch';
import Library from './src/components/Library';

export default function App() {
  const [user, setUser] = useState(null);

  const handleAuthStateChange = (userData) => {
    setUser(userData);
  };

  const handleLogout = async () => {
    const { error } = await supabase.auth.signOut();
    if (error) {
      console.error('Error signing out:', error);
    } else {
      setUser(null);
    }
  };

  return (
    <View style={styles.container}>
      {user ? (
        <View style={styles.contentContainer}>
          <View style={styles.header}>
            <Button title="Logout" onPress={handleLogout} />
          </View>
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
  header: {
    flexDirection: 'row',
    justifyContent: 'flex-end',
    marginBottom: 10,
  },
});