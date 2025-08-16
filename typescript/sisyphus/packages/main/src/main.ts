import { app, BrowserWindow, ipcMain } from 'electron';
import path from 'node:path';

// Import custom modules
import { createMainWindow, registerWindowEvents } from './app/window';
import { setupIPC } from './ipc';
import { loadMainConfig } from './config/configLoader';
import { applySecurityPolicies } from './security/securitySetup';

let mainWindow: BrowserWindow | undefined;

app.on('ready', async () => {
  // Load configuration (async for future-proofing)
  const config = await loadMainConfig();
  // Apply security (contextIsolation, CSP, etc.)
  applySecurityPolicies();
  // Create window, pass config if needed
  mainWindow = createMainWindow(config);
  // Register window event handlers (close, activate, etc.)
  registerWindowEvents(mainWindow);
  // IPC setup: give renderer access to APIs
  setupIPC(ipcMain, mainWindow, config);
});

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit();
});

app.on('activate', () => {
  if (BrowserWindow.getAllWindows().length === 0) {
    mainWindow = createMainWindow();
    registerWindowEvents(mainWindow);
  }
});