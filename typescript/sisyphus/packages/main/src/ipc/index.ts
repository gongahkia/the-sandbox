import { IpcMain, BrowserWindow } from 'electron';

export function setupIPC(ipcMain: IpcMain, mainWindow: BrowserWindow, config: any) {
  // Example: settings management
  ipcMain.handle('settings:get', async (_event, key?: string) => {
    if (key) return config[key];
    return config;
  });

  // Example: window control from renderer (allow-list only!)
  ipcMain.handle('window:minimize', () => mainWindow.minimize());
  ipcMain.handle('window:maximize', () => mainWindow.maximize());
  ipcMain.handle('window:close', () => mainWindow.close());

  // Add secure IPC handlers for file access, plugins, etc.
  // Always validate/sanitize data and restrict methods
}