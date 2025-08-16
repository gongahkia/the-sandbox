import React, { useRef, useEffect } from 'react';
import { Terminal } from 'xterm';
import 'xterm/css/xterm.css';
import { useTerminalTheme } from './themes/themeContext';

interface TerminalViewProps {
  fontSize?: number;
  theme?: string;
}

export default function TerminalView({ fontSize = 14, theme = 'dark' }: TerminalViewProps) {
  const containerRef = useRef<HTMLDivElement>(null);
  const terminalRef = useRef<Terminal | null>(null);
  const appliedTheme = useTerminalTheme(theme);

  useEffect(() => {
    if (!containerRef.current) return;

    const term = new Terminal({
      fontSize,
      theme: appliedTheme,
      rows: 24,
      cols: 80,
      cursorBlink: true,
    });

    term.open(containerRef.current);

    // Connect to PTY backend via IPC:
    window.electron?.ipcRenderer.on('terminal:data', (_event, data) => {
      term.write(data);
    });
    term.onData(data => {
      window.electron?.ipcRenderer.send('terminal:input', data);
    });

    terminalRef.current = term;
    return () => {
      term.dispose();
      window.electron?.ipcRenderer.removeAllListeners('terminal:data');
    };
  }, [fontSize, theme]);

  return <div ref={containerRef} style={{ width: '100%', height: '100%' }} />;
}