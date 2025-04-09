<template>
  <div class="chat-messages" ref="chatMessages">
    <div v-for="(message, index) in messages" :key="index" :class="message.role">
      {{ message.content }}
    </div>
  </div>
</template>

<script>
export default {
  props: {
    messages: {
      type: Array,
      required: true
    }
  },
  watch: {
    messages() {
      this.$nextTick(() => {
        this.$refs.chatMessages.scrollTop = this.$refs.chatMessages.scrollHeight;
      });
    }
  }
}
</script>

<style scoped>
.chat-messages {
  flex-grow: 1;
  overflow-y: auto;
  margin-bottom: 20px;
}

.chat-messages > div {
  margin-bottom: 10px;
  padding: 10px;
  border-radius: 5px;
}

.user {
  background-color: #e6f2ff;
  text-align: right;
}

.assistant {
  background-color: #f0f0f0;
}

.system {
  background-color: #ffe6e6;
  font-style: italic;
}
</style>