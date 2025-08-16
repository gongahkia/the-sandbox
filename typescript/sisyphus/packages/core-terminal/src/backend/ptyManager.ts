import { spawn, ChildProcessWithoutNullStreams } from 'node:child_process';

export class PTYManager {
  private pty: ChildProcessWithoutNullStreams | null = null;

  start(shell = process.env.SHELL || 'bash') {
    this.pty = spawn(shell, [], { stdio: 'pipe' });

    this.pty.on('data', (data: Buffer) => {
      // Send output via IPC to renderer
      // mainWindow.webContents.send('terminal:data', data.toString());
    });
  }

  write(input: string) {
    this.pty?.stdin.write(input);
  }

  dispose() {
    this.pty?.kill();
    this.pty = null;
  }
}