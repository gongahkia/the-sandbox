<script setup lang="ts">
import { ref } from 'vue'
import Editor from './components/Editor.vue'

const languages = [
  { name: 'JavaScript', value: 'javascript' },
  { name: 'Python', value: 'python' },
  { name: 'PHP', value: 'php' },
  { name: 'Go', value: 'go' }
]

const selectedLanguage = ref('javascript')
</script>

<template>
  <header>
    <h1>Scram</h1>
    <p>Run your code online.</p>
  </header>
  <div class="container">
    <select v-model="selectedLanguage">
      <option v-for="lang in languages" :value="lang.value">
        {{ lang.name }}
      </option>
    </select>
    <Editor :language="selectedLanguage" />
  </div>
  <footer>Made with ❤️ by <a href="https://gabrielongzm.com">Gabriel Ong</a>.<br>Source code <a href="https://github.com/gongahkia/scram">here</a>.</footer>
</template>

<style>
.container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 2rem;
}

select {
  padding: 0.5rem;
  margin-bottom: 1rem;
  background: #1e1e1e;
  color: white;
  border: 1px solid #434343;
  border-radius: 4px;
}

header, footer {
  text-align: center;
  padding: 1rem;
  color: #888;
}

footer a {
  color: #007acc;
  text-decoration: none;
}
</style>