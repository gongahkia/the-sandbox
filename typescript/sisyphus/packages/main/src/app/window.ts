import { BrowserWindow } from 'electron';
import path from 'node:path';

export function createMainWindow(config?: any): BrowserWindow {
  const win = new BrowserWindow({
    width: config?.window?.width || 1280,
    height: config?.window?.height || 800,
    minWidth: 900,
    minHeight: 600,
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true,
      preload: path.join(__dirname, '../ipc/preload.js'),
      devTools: config?.devTools ?? true,
    },
    show: false,
  });

  // Only show after ready to avoid white flash
  win.once('ready-to-show', () => win.show());

  // Load renderer index
  win.loadURL(config?.rendererUrl ?? `file://${path.join(__dirname, '../../renderer/dist/index.html')}`);

  return win;
}

export function registerWindowEvents(win: BrowserWindow) {
  win.on('closed', () => {
    // Dereference window for GC
  });
  win.on('resize', () => {
    // Save window size to config if needed
  });
  // Add more app events as required
}