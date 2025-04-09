<template>
  <div>
    <h2>Password Generator</h2>
    <input v-model="password" readonly />
    <button @click="generatePassword">Generate Password</button>
    <input v-model="description" placeholder="Password description" />
    <button @click="savePassword">Save Password</button>
    <div v-if="message">{{ message }}</div>
    <h3>Saved Passwords</h3>
    <ul>
      <li v-for="pwd in savedPasswords" :key="pwd.id">
        {{ pwd.description }}: {{ pwd.password }}
      </li>
    </ul>
  </div>
</template>

<script>
import { ref, onMounted } from 'vue'
import { supabase } from '../lib/supabaseClient'

export default {
  setup() {
    const password = ref('')
    const description = ref('')
    const message = ref('')
    const savedPasswords = ref([])

    const generatePassword = () => {
      const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*'
      let result = ''
      for (let i = 0; i < 12; i++) {
        result += chars.charAt(Math.floor(Math.random() * chars.length))
      }
      password.value = result
    }

const savePassword = async () => {
  if (!password.value || !description.value) {
    message.value = 'Please generate a password and provide a description.'
    return
  }

    const { error } = await supabase
      .from('passwords')
      .insert({ password: password.value, description: description.value })
    
    if (error) {
      console.error('Error saving password:', error)
      message.value = 'Error saving password. Please try again.'
    } else {
      message.value = 'Password saved successfully'
      fetchPasswords()
      password.value = ''
      description.value = ''
    }
  }

    const fetchPasswords = async () => {
      const { data, error } = await supabase
        .from('passwords')
        .select('*')
      
      if (error) {
        console.error('Error fetching passwords:', error)
      } else {
        savedPasswords.value = data
      }
    }

    onMounted(() => {
      fetchPasswords()
    })

    return {
      password,
      description,
      message,
      savedPasswords,
      generatePassword,
      savePassword
    }
  }
}
</script>