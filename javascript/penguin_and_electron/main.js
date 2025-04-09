const { app, BrowserWindow, ipcMain } = require('electron');
const path = require('path');
const axios = require('axios');

function createWindow() {
  const win = new BrowserWindow({
    width: 1200,
    height: 800,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js')
    }
  });

  win.loadFile('index.html');
}

app.whenReady().then(createWindow);

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit();
});

app.on('activate', () => {
  if (BrowserWindow.getAllWindows().length === 0) createWindow();
});

ipcMain.handle('search-books', async (event, query) => {
  try {
    const response = await axios.get(`https://reststop.randomhouse.com/resources/works?search=${query}`, {
      auth: {
        username: 'testuser',
        password: 'testpassword'
      }
    });
    return response.data;
  } catch (error) {
    console.error('Error searching books:', error);
    return { error: 'Failed to search books' };
  }
});

ipcMain.handle('get-book-details', async (event, workId) => {
  try {
    const response = await axios.get(`https://reststop.randomhouse.com/resources/works/${workId}`, {
      auth: {
        username: 'testuser',
        password: 'testpassword'
      }
    });
    return response.data;
  } catch (error) {
    console.error('Error fetching book details:', error);
    return { error: 'Failed to fetch book details' };
  }
});