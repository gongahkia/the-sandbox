const { contextBridge, ipcRenderer } = require('electron');

contextBridge.exposeInMainWorld('electronAPI', {
  searchBooks: (query) => ipcRenderer.invoke('search-books', query),
  getBookDetails: (workId) => ipcRenderer.invoke('get-book-details', workId)
});