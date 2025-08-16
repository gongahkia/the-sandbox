<template>
  <div id="app">
    <h1></h1>
    <BookSearch />
  </div>
</template>

<script>
import BookSearch from './components/BookSearch.vue'

export default {
  name: 'App',
  components: {
    BookSearch
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