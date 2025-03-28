import * as SecureStore from "expo-secure-store";

const EXPO_PUBLIC_CLERK_PUBLISHABLE_KEY = "XXX"

const tokenCache = {
  async getToken(key: string) {
    try {
      return SecureStore.getItemAsync(key);
    } catch (err) {
      return null;
    }
  },
  async saveToken(key: string, value: string) {
    try {
      return SecureStore.setItemAsync(key, value);
    } catch (err) {
      return;
    }
  },
};

export const clerkPublishableKey = EXPO_PUBLIC_CLERK_PUBLISHABLE_KEY;
export { tokenCache };