const { contextBridge, ipcRenderer } = require('electron');

contextBridge.exposeInMainWorld('electronAPI', {
  validatePython: (code) => ipcRenderer.invoke('validate-python', code)
});