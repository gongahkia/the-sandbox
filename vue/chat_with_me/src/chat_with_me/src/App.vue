<template>
  <div id="app">
    <ChatInterface />
  </div>
</template>

<script>
import ChatInterface from './components/ChatInterface.vue';

export default {
  name: 'App',
  components: {
    ChatInterface
  }
}
</script>

<style>
#app {
  font-family: Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  color: #2c3e50;
  margin-top: 60px;
}
</style>