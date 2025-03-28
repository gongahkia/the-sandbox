<template>
  <q-page class="editor-container">
    <div class="editor-pane">
      <div class="toolbar">
        <q-btn-group flat>
          <q-btn :color="editorMode === 'markdown' ? 'primary' : ''" label="Markdown" @click="setEditorMode('markdown')" />
          <q-btn :color="editorMode === 'latex' ? 'primary' : ''" label="LaTeX" @click="setEditorMode('latex')" />
        </q-btn-group>
        <q-space />
        <q-toggle v-model="isVimMode" label="Vim Mode" />
        <q-chip v-if="isVimMode" :color="vimModeColor" text-color="white">
          {{ vimState.mode.toUpperCase() }}
        </q-chip>
      </div>
      <q-editor
        ref="editorRef"
        v-model="editorContent"
        :toolbar="[]"
        class="editor-content"
        @keydown="handleEditorKeydown"
      />
    </div>
    <div class="preview-pane">
      <div v-if="editorMode === 'markdown'" class="markdown-preview">
        <q-markdown :src="editorContent" />
      </div>
      <div v-else-if="editorMode === 'latex'" class="latex-preview">
        <div v-html="renderedLatex"></div>
      </div>
    </div>
  </q-page>
</template>

<script setup lang="ts">
import { ref, computed, onMounted } from 'vue';
import { useVimMode } from 'src/composables/useVimMode';
import { useLatexRenderer } from 'src/composables/useLatexRenderer';

const editorRef = ref(null);
const editorContent = ref('# Welcome to LaTeX & Markdown Editor\n\nStart typing your content here.\n\n## Math Example\n\n$$E = mc^2$$\n\nInline equation: $a^2 + b^2 = c^2$');
const editorMode = ref('markdown');

// Vim mode integration
const { isVimMode, vimState, handleKeydown } = useVimMode();

// LaTeX renderer integration
const { renderedLatex } = useLatexRenderer(editorContent);

// Computed properties
const vimModeColor = computed(() => {
  switch (vimState.mode) {
    case 'normal':
      return 'blue';
    case 'insert':
      return 'green';
    case 'visual':
      return 'purple';
    default:
      return 'grey';
  }
});

// Methods
function setEditorMode(mode) {
  editorMode.value = mode;
}

function handleEditorKeydown(event:KeyboardEvent) {
  if (isVimMode.value) {
    return handleKeydown(event, editorRef.value);
  }
  return true;
}

// File operations
function newDocument() {
  editorContent.value = '';
}

function saveDocument() {
  const blob = new Blob([editorContent.value], { type: 'text/plain' });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = `document.${editorMode.value === 'markdown' ? 'md' : 'tex'}`;
  a.click();
  URL.revokeObjectURL(url);
}

function openDocument() {
  const input = document.createElement('input');
  input.type = 'file';
  input.accept = '.md,.tex';
  input.onchange = (event) => {
    const file = event.target.files[0];
    if (file) {
      const reader = new FileReader();
      reader.onload = (e) => {
        editorContent.value = e.target.result;
        // Set editor mode based on file extension
        if (file.name.endsWith('.tex')) {
          editorMode.value = 'latex';
        } else {
          editorMode.value = 'markdown';
        }
      };
      reader.readAsText(file);
    }
  };
  input.click();
}

// Keyboard shortcuts
function setupKeyboardShortcuts() {
  document.addEventListener('keydown', (event:KeyboardEvent) => {
    // Check for Ctrl+S or Cmd+S (save)
    if ((event.ctrlKey || event.metaKey) && event.key === 's') {
      event.preventDefault();
      saveDocument();
    }
    // Check for Ctrl+O or Cmd+O (open)
    if ((event.ctrlKey || event.metaKey) && event.key === 'o') {
      event.preventDefault();
      openDocument();
    }
    // Check for Ctrl+N or Cmd+N (new)
    if ((event.ctrlKey || event.metaKey) && event.key === 'n') {
      event.preventDefault();
      newDocument();
    }
  });
}

// Lifecycle hooks
onMounted(() => {
  setupKeyboardShortcuts();
});
</script>

<style lang="scss">
.editor-container {
  display: flex;
  height: calc(100vh - 50px);
  overflow: hidden;
}

.editor-pane, .preview-pane {
  flex: 1;
  overflow: auto;
  padding: 1rem;
  display: flex;
  flex-direction: column;
}

.editor-pane {
  border-right: 1px solid #ddd;
}

.toolbar {
  display: flex;
  padding-bottom: 8px;
  border-bottom: 1px solid #eee;
  margin-bottom: 8px;
  align-items: center;
}

.editor-content {
  flex: 1;
  font-family: 'Fira Code', monospace;
  font-size: 14px;
  line-height: 1.5;
  min-height: 300px;
}

.markdown-preview, .latex-preview {
  padding: 1rem;
  background: #f9f9f9;
  border-radius: 4px;
  flex: 1;
  overflow: auto;
}

.error {
  color: red;
  background: #ffeeee;
  padding: 4px;
  border-radius: 4px;
}

/* Vim cursor styles */
.vim-cursor-normal {
  border-left: 2px solid #000;
}

.vim-cursor-insert {
  border-left: 2px solid #4CAF50;
}

.vim-cursor-visual {
  background-color: rgba(65, 105, 225, 0.3);
}
</style>