import React from 'react';
import { View, Text } from 'react-native';
import { ClerkProvider, SignedIn, SignedOut, useUser } from '@clerk/clerk-expo';
import { clerkPublishableKey, tokenCache } from './lib/clerk';
import Auth from './components/Auth';
import StepCounter from './components/StepCounter';

export default function App() {
  return (
    <ClerkProvider publishableKey={clerkPublishableKey} tokenCache={tokenCache}>
      <View style={{ flex: 1, justifyContent: 'center', alignItems: 'center' }}>
        <SignedIn>
          <SignedInContent />
        </SignedIn>
        <SignedOut>
          <Auth />
        </SignedOut>
      </View>
    </ClerkProvider>
  );
}

function SignedInContent() {
  const { user } = useUser();
  return (
    <View>
      <Text>Welcome, {user?.firstName}!</Text>
      <StepCounter userId={user?.id || ''} />
    </View>
  );
}