const { Application } = require('spectron');
const electronPath = require('electron'); // Path to electron

let app: any;

describe('Sisyphus IDE App', () => {
  beforeAll(async () => {
    app = new Application({
      path: electronPath,
      args: ['.'] // run root of project
    });
    await app.start();
  });

  afterAll(async () => {
    if (app && app.isRunning()) await app.stop();
  });

  it('opens main window', async () => {
    const winCount = await app.client.getWindowCount();
    expect(winCount).toBeGreaterThan(0);
  });

  it('applies user theme to window', async () => {
    const themeType = await app.client.execute(() =>
      document.body.getAttribute('data-theme')
    );
    expect(['light', 'dark']).toContain(themeType.value);
  });
});
