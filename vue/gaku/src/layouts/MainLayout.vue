<template>
  <q-layout view="hHh LpR fFf">
    <q-header elevated>
      <q-toolbar>
        <q-btn flat dense round icon="menu" @click="toggleLeftDrawer" />
        <q-toolbar-title>LaTeX & Markdown Editor</q-toolbar-title>
        <q-btn-group flat>
          <q-btn label="New" icon="add" @click="newDocument" />
          <q-btn label="Open" icon="folder_open" @click="openDocument" />
          <q-btn label="Save" icon="save" @click="saveDocument" />
        </q-btn-group>
      </q-toolbar>
    </q-header>

    <q-drawer v-model="leftDrawerOpen" bordered>
      <q-list>
        <q-item-label header>Recent Documents</q-item-label>
        <q-item v-for="(doc, index) in recentDocuments" :key="index" clickable @click="loadDocument(doc)">
          <q-item-section>
            <q-item-label>{{ doc.name }}</q-item-label>
            <q-item-label caption>{{ doc.lastEdited }}</q-item-label>
          </q-item-section>
        </q-item>
      </q-list>
    </q-drawer>

    <q-page-container>
      <router-view />
    </q-page-container>
  </q-layout>
</template>
<script lang="ts">
import { ref, inject, Ref } from 'vue';
import { useRouter } from 'vue-router';
import { useQuasar } from 'quasar';

interface Document {
  name: string;
  lastEdited: string;
  content: string;
  type: 'markdown' | 'latex';
}

const $q = useQuasar();
const router = useRouter();

const editorContent = inject<Ref<string>>('editorContent', ref(''));
const editorMode = inject<Ref<'markdown' | 'latex'>>('editorMode', ref('markdown'));

const leftDrawerOpen = ref(false);
const recentDocuments = ref([
  { name: 'Document 1.md', lastEdited: '2 hours ago', content: '# Sample Markdown\n\nThis is a sample markdown document.', type: 'markdown' },
  { name: 'Math Paper.tex', lastEdited: 'Yesterday', content: '\\documentclass{article}\n\\begin{document}\nE = mc^2\n\\end{document}', type: 'latex' }
]);

// Load recent documents from localStorage if available
function loadRecentDocumentsFromStorage(): void {
  try {
    const storedDocs = localStorage.getItem('recentDocuments');
    if (storedDocs) {
      recentDocuments.value = JSON.parse(storedDocs);
    }
  } catch (e) {
    console.error('Error loading recent documents:', e);
  }
}

// Save recent documents to localStorage
function saveRecentDocumentsToStorage(): void {
  try {
    localStorage.setItem('recentDocuments', JSON.stringify(recentDocuments.value));
  } catch (e) {
    console.error('Error saving recent documents:', e);
  }
}

function addToRecentDocuments(doc: Omit<Document, 'lastEdited'>): void {
  const existingIndex = recentDocuments.value.findIndex(d => d.name === doc.name);
  if (existingIndex !== -1) {
    recentDocuments.value.splice(existingIndex, 1);
  }
  recentDocuments.value.unshift({
    ...doc,
    lastEdited: 'Just now'
  });
  if (recentDocuments.value.length > 10) {
    recentDocuments.value = recentDocuments.value.slice(0, 10);
  }
  saveRecentDocumentsToStorage();
}

function toggleLeftDrawer(): void {
  leftDrawerOpen.value = !leftDrawerOpen.value;
}

function newDocument(): void {
  // Confirm if there are unsaved changes
  if (editorContent.value.trim() !== '') {
    $q.dialog({
      title: 'Unsaved Changes',
      message: 'You have unsaved changes. Do you want to continue?',
      cancel: true,
      persistent: true
    }).onOk(() => {
      // Reset editor content and navigate to editor
      editorContent.value = '';
      router.push('/');
      $q.notify({
        message: 'New document created',
        color: 'positive'
      });
    });
  } else {
    // No changes, just reset
    editorContent.value = '';
    router.push('/');
  }
}

function openDocument(): void {
  // Create a file input element
  const input = document.createElement('input');
  input.type = 'file';
  input.accept = '.md,.markdown,.tex';
  
  input.onchange = (event: Event) => {
    const target = event.target as HTMLInputElement;
    const file = target.files?.[0];
    if (!file) return;
    
    // Check file extension
    const fileName = file.name;
    const fileExt = fileName.split('.').pop()?.toLowerCase() || '';
    
    if (!['md', 'markdown', 'tex'].includes(fileExt)) {
      $q.notify({
        message: 'Only Markdown (.md, .markdown) and LaTeX (.tex) files are supported',
        color: 'negative'
      });
      return;
    }
    
    // Read file
    const reader = new FileReader();
    reader.onload = (e: ProgressEvent<FileReader>) => {
      const content = e.target?.result as string;
      if (!content) return;
      
      // Set editor content and mode based on file type
      editorContent.value = content;
      editorMode.value = fileExt === 'tex' ? 'latex' : 'markdown';
      
      // Add to recent documents
      addToRecentDocuments({
        name: fileName,
        content: content,
        type: fileExt === 'tex' ? 'latex' : 'markdown'
      });
      
      $q.notify({
        message: `Opened ${fileName}`,
        color: 'positive'
      });
      
      // Navigate to editor
      router.push('/');
    };
    
    reader.onerror = () => {
      $q.notify({
        message: 'Error reading file',
        color: 'negative'
      });
    };
    
    reader.readAsText(file);
  };
  
  // Trigger file selection
  input.click();
}

function saveDocument(): void {
  // Create dialog to get filename
  $q.dialog({
    title: 'Save Document',
    message: 'Enter a filename:',
    prompt: {
      model: `document.${editorMode.value === 'latex' ? 'tex' : 'md'}`,
      type: 'text'
    },
    cancel: true,
    persistent: true
  }).onOk((filename: string) => {
    // Ensure proper extension
    let finalFilename = filename;
    const mode = editorMode.value;
    
    if (mode === 'markdown' && !finalFilename.endsWith('.md') && !finalFilename.endsWith('.markdown')) {
      finalFilename += '.md';
    } else if (mode === 'latex' && !finalFilename.endsWith('.tex')) {
      finalFilename += '.tex';
    }
    
    // Create blob and download
    const blob = new Blob([editorContent.value], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = finalFilename;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
    
    // Add to recent documents
    addToRecentDocuments({
      name: finalFilename,
      content: editorContent.value,
      type: mode
    });
    
    $q.notify({
      message: `Saved as ${finalFilename}`,
      color: 'positive'
    });
  });
}

function loadDocument(doc: Document): void {
  // Confirm if there are unsaved changes
  if (editorContent.value.trim() !== '' && editorContent.value !== doc.content) {
    $q.dialog({
      title: 'Unsaved Changes',
      message: 'You have unsaved changes. Do you want to continue?',
      cancel: true,
      persistent: true
    }).onOk(() => {
      // Load document content
      editorContent.value = doc.content;
      editorMode.value = doc.type;
      leftDrawerOpen.value = false;
      
      // Update last edited time
      const docIndex = recentDocuments.value.findIndex(d => d.name === doc.name);
      if (docIndex !== -1) {
        recentDocuments.value[docIndex].lastEdited = 'Just now';
        saveRecentDocumentsToStorage();
      }
      
      $q.notify({
        message: `Loaded ${doc.name}`,
        color: 'positive'
      });
    });
  } else {
    // No changes or same document, just load
    editorContent.value = doc.content;
    editorMode.value = doc.type;
    leftDrawerOpen.value = false;
    
    // Update last edited time
    const docIndex = recentDocuments.value.findIndex(d => d.name === doc.name);
    if (docIndex !== -1) {
      recentDocuments.value[docIndex].lastEdited = 'Just now';
      saveRecentDocumentsToStorage();
    }
    
    $q.notify({
      message: `Loaded ${doc.name}`,
      color: 'positive'
    });
  }
}

loadRecentDocumentsFromStorage();
</script>