export function useExplorerActions() {
  return {
    openFile: (path: string) => {
      window.electron?.ipcRenderer.send('explorer:openFile', path);
    },
    deleteFile: (path: string) => {
      window.electron?.ipcRenderer.send('explorer:deleteFile', path);
    },
    renameFile: (path: string, newName: string) => {
      window.electron?.ipcRenderer.send('explorer:renameFile', { path, newName });
    },
  };
}