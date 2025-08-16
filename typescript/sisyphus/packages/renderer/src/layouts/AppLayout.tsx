import React from 'react';
import AppHeader from '../components/AppHeader';
import EditorWrapper from '../components/EditorWrapper'; // Wraps core-editor integration
import TerminalPanel from '../components/TerminalPanel'; // Wraps core-terminal
import FileTreeExplorer from '../components/FileTreeExplorer'; // core-explorer wrapper
import ThemeSwitcher from '../components/ThemeSwitcher';
import './AppLayout.css';

export default function AppLayout() {
  return (
    <div className="app-root">
      <AppHeader />
      <div className="main-layout">
        <aside className="sidebar">
          <FileTreeExplorer />
        </aside>
        <main className="editor-area">
          <EditorWrapper />
          <TerminalPanel />
        </main>
        <ThemeSwitcher />
      </div>
    </div>
  );
}