<template>
  <div class="chat-input">
    <input v-model="userInput" @keyup.enter="sendMessage" placeholder="Type your message...">
    <button @click="sendMessage">Send</button>
  </div>
</template>

<script>
export default {
  data() {
    return {
      userInput: ''
    }
  },
  methods: {
    sendMessage() {
      if (!this.userInput.trim()) return;
      this.$emit('send-message', this.userInput);
      this.userInput = '';
    }
  }
}
</script>

<style scoped>
.chat-input {
  display: flex;
}

input {
  flex-grow: 1;
  padding: 10px;
  font-size: 16px;
}

button {
  padding: 10px 20px;
  font-size: 16px;
  background-color: #4CAF50;
  color: white;
  border: none;
  cursor: pointer;
}
</style>