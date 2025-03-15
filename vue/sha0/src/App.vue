<template>
  <n-layout style="height: 100vh; display: flex; flex-direction: column;">
    <!-- Header -->
    <n-layout-header style="text-align: center; padding: 20px; background-color: #f5f5f5;">
      <h1>Password Generator</h1>
    </n-layout-header>

    <!-- Content -->
    <n-layout-content style="padding: 20px;">
      <n-card title="Generate a Custom Password" style="max-width: 600px; margin: auto;">
        <n-form :model="form" label-placement="top">
          <!-- Password Length -->
          <n-form-item label="Password Length">
            <n-input-number v-model:value="form.length" :min="4" :max="32" />
          </n-form-item>

          <!-- Password Type Options -->
          <n-form-item label="Password Type">
            <n-radio-group v-model:value="form.passwordType">
              <n-radio value="letters">Letters Only</n-radio>
              <n-radio value="digits">Digits Only</n-radio>
              <n-radio value="lettersAndDigits">Letters and Digits</n-radio>
              <n-radio value="lettersDigitsSpecial">Letters, Digits, and Special Characters</n-radio>
            </n-radio-group>
          </n-form-item>

          <!-- Include Uppercase Letters -->
          <n-form-item label="Include Uppercase Letters">
            <n-switch v-model:value="form.includeUppercase" />
          </n-form-item>

          <!-- Generate and Copy Buttons -->
          <n-space justify="center" style="margin-top: 20px;">
            <n-button type="primary" @click="generatePassword">Generate Password</n-button>
            <n-button type="default" @click="copyToClipboard">Copy to Clipboard</n-button>
          </n-space>
        </n-form>

        <!-- Display Generated Password -->
        <div v-if="password" style="margin-top: 20px; text-align: center;">
          <h3>Generated Password:</h3>
          <p>{{ password }}</p>
        </div>
      </n-card>
    </n-layout-content>
  </n-layout>
</template>

<script setup>
import { ref } from 'vue';

const form = ref({
  length: 12,
  passwordType: 'lettersDigitsSpecial', // Default type
  includeUppercase: true,
});

const password = ref('');

const generatePassword = () => {
  const length = form.value.length;
  let charset = '';

  // Determine charset based on user selection
  switch (form.value.passwordType) {
    case 'letters':
      charset = 'abcdefghijklmnopqrstuvwxyz';
      if (form.value.includeUppercase) charset += 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
      break;
    case 'digits':
      charset = '0123456789';
      break;
    case 'lettersAndDigits':
      charset = 'abcdefghijklmnopqrstuvwxyz0123456789';
      if (form.value.includeUppercase) charset += 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
      break;
    case 'lettersDigitsSpecial':
      charset = 'abcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()_+[]{}|;:,.<>?';
      if (form.value.includeUppercase) charset += 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
      break;
  }

  // Generate password
  let generatedPassword = '';
  for (let i = 0; i < length; i++) {
    const randomIndex = Math.floor(Math.random() * charset.length);
    generatedPassword += charset[randomIndex];
  }
  password.value = generatedPassword;
};

const copyToClipboard = () => {
  navigator.clipboard.writeText(password.value).then(() => {
    alert('Password copied to clipboard!');
  });
};
</script>

<style scoped>
body {
  font-family: Arial, sans-serif;
}
h1 {
  font-size: 24px;
}
p {
  font-size: 18px;
}
</style>