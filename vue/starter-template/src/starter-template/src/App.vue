<template>
  <div id="app">
    <h1>Vue.js Higher Lower Game</h1>
    <HigherLowerGame />
  </div>
</template>

<script>
import HigherLowerGame from './components/HigherLowerGame.vue'

export default {
  name: 'App',
  components: {
    HigherLowerGame
  }
}
</script>

<style>
#app {
  font-family: Arial, sans-serif;
  text-align: center;
  margin-top: 50px;
}
</style>