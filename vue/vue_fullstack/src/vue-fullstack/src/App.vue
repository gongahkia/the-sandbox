<template>
  <div id="app">
    <h1>Learning Vue fullstack with a simple password generator</h1>
    <PasswordGenerator />
  </div>
</template>

<script>
import PasswordGenerator from './components/PasswordGenerator.vue'

export default {
  name: 'App',
  components: {
    PasswordGenerator
  }
}
</script>