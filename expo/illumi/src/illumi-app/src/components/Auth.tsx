import React, { useState } from 'react';
import { Alert, StyleSheet, View } from 'react-native';
import { Button, Input } from '@rneui/themed';
import { supabase } from '../lib/supabase';

export default function Auth({ onAuthStateChange }) {
  const [username, setUsername] = useState('');
  const [password, setPassword] = useState('');
  const [loading, setLoading] = useState(false);

  async function signIn() {
    setLoading(true);
    const { data, error } = await supabase
      .from('user_auth')
      .select('id, username')
      .eq('username', username)
      .eq('password', password)
      .single();

    if (error) Alert.alert('Error', error.message);
    else if (data) {
      onAuthStateChange(data);
    } else {
      Alert.alert('Error', 'Invalid credentials');
    }
    setLoading(false);
  }

  async function signUp() {
    setLoading(true);
    const { data, error } = await supabase
      .from('user_auth')
      .insert({ username, password })
      .select('id')
      .single();

    if (error) Alert.alert('Error', error.message);
    else if (data) {
      await supabase.from('user_profiles').insert({ id: data.id, username });
      Alert.alert('Success', 'Account created successfully');
      onAuthStateChange(data);
    }
    setLoading(false);
  }

  return (
    <View style={styles.container}>
      <Input
        label="Username"
        onChangeText={setUsername}
        value={username}
        placeholder="Enter username"
        autoCapitalize={'none'}
      />
      <Input
        label="Password"
        onChangeText={setPassword}
        value={password}
        secureTextEntry={true}
        placeholder="Enter password"
        autoCapitalize={'none'}
      />
      <View style={[styles.verticallySpaced, styles.mt20]}>
        <Button title="Sign in" disabled={loading} onPress={signIn} />
      </View>
      <View style={styles.verticallySpaced}>
        <Button title="Sign up" disabled={loading} onPress={signUp} />
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    marginTop: 40,
    padding: 12,
  },
  verticallySpaced: {
    paddingTop: 4,
    paddingBottom: 4,
    alignSelf: 'stretch',
  },
  mt20: {
    marginTop: 20,
  },
});