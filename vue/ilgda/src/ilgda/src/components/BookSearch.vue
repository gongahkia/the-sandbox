<template>
  <div class="book-search">
    <h2>읽다 more books</h2>
    <input v-model="searchQuery" @keyup.enter="searchBooks" placeholder="Enter book title">
    <button @click="searchBooks">Search</button>
    <div v-if="loading">Loading...</div>
    <ul v-else>
      <li v-for="book in books" :key="book.key">
        <h3>{{ book.title }}</h3>
        <p>Author(s): {{ book.author_name ? book.author_name.join(', ') : 'Unknown' }}</p>
        <p>First Published: {{ book.first_publish_year || 'Unknown' }}</p>
      </li>
    </ul>
  </div>
</template>

<script>
export default {
  name: 'BookSearch',
  data() {
    return {
      searchQuery: '',
      books: [],
      loading: false
    }
  },
  methods: {
    searchBooks() {
      this.loading = true;
      fetch(`https://openlibrary.org/search.json?q=${encodeURIComponent(this.searchQuery)}`)
        .then(response => response.json())
        .then(data => {
          this.books = data.docs.slice(0, 10); // Display first 10 results
          this.loading = false;
        })
        .catch(error => {
          console.error('Error:', error);
          this.loading = false;
        });
    }
  }
}
</script>

<style scoped>
.book-search {
  font-family: Arial, sans-serif;
  max-width: 800px;
  margin: 0 auto;
  padding: 20px;
}
input, button {
  margin: 10px 0;
  padding: 5px;
}
ul {
  list-style-type: none;
  padding: 0;
}
li {
  border: 1px solid #ddd;
  margin: 10px 0;
  padding: 10px;
}
</style>