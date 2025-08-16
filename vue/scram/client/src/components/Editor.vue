<script setup lang="ts">
import { ref, watch, onMounted } from 'vue'
import { VueMonacoEditor } from '@guolao/vue-monaco-editor'
import axios from 'axios'

declare global {
  interface Window {
    loadPyodide: any
    pyodide: any
  }
}

let pyodide: any = null
async function loadPyodide() {
  if (!pyodide) {
    pyodide = await window.loadPyodide({
      indexURL: "https://cdn.jsdelivr.net/pyodide/v0.25.0/full/",
      stdout: (msg: string) => {
        output.value += msg
      },
      stderr: (msg: string) => {
        error.value += msg
      }
    })
  }
  return pyodide
}

const props = defineProps({
  language: { type: String, required: true }
})

const code = ref('')
const output = ref('')
const error = ref('')

onMounted(async () => {
  if (props.language === 'python') {
    await loadPyodide()
  }
})

watch(() => props.language, async (newLang) => {
  code.value = ''
  output.value = ''
  error.value = ''
  if (newLang === 'python') {
    await loadPyodide()
  }
})

const executeCode = async () => {
  output.value = ''
  error.value = ''
  try {
    if (props.language === 'javascript') {
      const logs: string[] = []
      const mockConsole = {
        log: (...args: any[]) => logs.push(args.join(' ')),
        error: (...args: any[]) => logs.push('ERROR: ' + args.join(' '))
      }

      // Corrected JavaScript execution
      const func = new Function(
        'console',
        `"use strict";
        try {
          ${code.value}
        } catch(e) {
          console.error(e instanceof Error ? e.message : String(e));
          throw e;
        }`
      )

      func(mockConsole)
      output.value = logs.join('\n') || 'Code executed (no output)'

    } else if (props.language === 'python') {
      await loadPyodide()
      const result = pyodide.runPython(code.value)
      output.value = result !== undefined ? String(result) : 'Code executed (no output)'
      
    } else {
      const { data } = await axios.post('/api/execute', {
        language: props.language,
        code: code.value
      })
      output.value = data.output || 'No output'
    }
  } catch (e: any) {
    error.value = e.stack || e.message || String(e)
    if (!error.value.includes('Error')) {
      error.value = `Error: ${error.value}`
    }
  }
}
</script>

<template>
  <div class="editor-container">
    <VueMonacoEditor
      v-model="code"
      :language="language"
      theme="vs-dark"
      :options="{
        minimap: { enabled: false },
        automaticLayout: true,
        scrollBeyondLastLine: false,
        fontSize: 14
      }"
      style="height: 500px; width: 100%; border: 1px solid #434343;"
    />
    <button class="run-button" @click="executeCode">Run Code</button>
    
    <div v-if="output" class="output-container">
      <h4>Output:</h4>
      <pre class="output-content">{{ output }}</pre>
    </div>
    
    <div v-if="error" class="error-container">
      <h4>Error:</h4>
      <pre class="error-content">{{ error }}</pre>
    </div>
  </div>
</template>

<style scoped>
.editor-container {
  display: flex;
  flex-direction: column;
  gap: 1rem;
  width: 100%;
  margin: 0 auto;
}

.run-button {
  align-self: flex-start;
  padding: 0.75rem 1.5rem;
  background: #007acc;
  color: white;
  border: none;
  border-radius: 4px;
  cursor: pointer;
  transition: background 0.2s;
  font-weight: 500;
}

.run-button:hover {
  background: #0062a3;
}

.output-container,
.error-container {
  padding: 1rem;
  border-radius: 4px;
  white-space: pre-wrap;
}

.output-container {
  background: #1e1e1e;
  border: 1px solid #434343;
}

.error-container {
  background: #2a1d1d;
  border: 1px solid #ff4d4d;
}

.output-content,
.error-content {
  margin: 0;
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.9em;
}

.error-content {
  color: #ff4d4d;
}

h4 {
  margin: 0 0 0.5rem 0;
  color: #858585;
  font-size: 0.9em;
}
</style>