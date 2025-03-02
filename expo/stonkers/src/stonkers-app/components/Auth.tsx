import React from 'react';
import { View, Button } from 'react-native';
import { useSignIn } from '@clerk/clerk-expo';

export default function Auth() {
  const { signIn, isLoaded } = useSignIn();

  const onSignInPress = async () => {
    if (!isLoaded) {
      return;
    }

    try {
      const completeSignIn = await signIn.create({
        identifier: 'user@example.com',
        password: 'password',
      });
      await completeSignIn.completeSignIn();
    } catch (err: any) {
      console.error('Error signing in', err);
    }
  };

  return (
    <View>
      <Button title="Sign In" onPress={onSignInPress} />
    </View>
  );
}