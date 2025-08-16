export interface FileNode {
  name: string;
  path: string;
  isFile: boolean;
  ext?: string;
}

export async function fetchDirectory(root: string): Promise<FileNode[]> {
  // Uses secure IPC to main process
  return window.electron?.ipcRenderer.invoke('explorer:fetchDir', root) || [];
}
