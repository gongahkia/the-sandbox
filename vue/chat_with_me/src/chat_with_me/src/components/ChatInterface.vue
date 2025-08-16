<template>
  <div class="chat-container">
    <ChatMessages :messages="messages" />
    <ChatInput @send-message="sendMessage" />
  </div>
</template>

<script>
import { ChatWebLLM } from '@langchain/community/chat_models/webllm';
import * as webllm from "@mlc-ai/web-llm";
import ChatMessages from './ChatMessages.vue';
import ChatInput from './ChatInput.vue';
import OpenAI from 'openai';

const openai = new OpenAI({
  apiKey: import.meta.env.VITE_OPENAI_API_KEY
});

export default {
  components: {
    ChatMessages,
    ChatInput
  },
  data() {
    return {
      model: null,
      messages: [],
    };
  },

  async mounted() {
    this.model = new ChatWebLLM({
      model: "Phi-3-mini-4k-instruct-q4f16_1-MLC",
      webllm: webllm,
      chatOptions: {
        temperature: 0.7,
      },
    });

    await this.model.initialize((progress) => {
      console.log('Loading model:', progress);
    });

    this.addMessage('system', 'Chat initialized. How can I help you today?');
  },

  methods: {
    addMessage(role, content) {
      this.messages.push({ role, content });
    },

    async sendMessage(userInput) {
      this.addMessage('user', userInput);
      try {
        const stream = await openai.chat.completions.create({
          model: "gpt-3.5-turbo",
          messages: [{ role: 'user', content: userInput }],
          stream: true,
        });
        let aiReply = '';
        for await (const chunk of stream) {
          const content = chunk.choices[0]?.delta?.content || '';
          aiReply += content;
          this.updateLastMessage('assistant', aiReply);
        }
      } catch (error) {
        console.error('Error generating response:', error);
        this.addMessage('system', 'Sorry, there was an error generating the response.');
      }
    },

    updateLastMessage(role, content) {
      if (this.messages.length > 0) {
        const lastMessage = { ...this.messages[this.messages.length - 1], role, content };
        this.$set(this.messages, this.messages.length - 1, lastMessage);
      } else {
        this.addMessage(role, content);
      }
    },

  },
};
</script>

<style scoped>
.chat-container {
  display: flex;
  flex-direction: column;
  height: 100vh;
  max-width: 600px;
  margin: 0 auto;
  padding: 20px;
}
</style>