import React from 'react';
import { pluginAPI } from '@sisyphus/core-plugins/src/api/pluginApi';

export function HelloWorldPanel() {
  return <div style={{ padding: 16 }}>Hello, Sisyphus World 🌏</div>;
}

// Register Hello World panel at plugin load time
pluginAPI.registerPanel('hello-world-panel', HelloWorldPanel);
pluginAPI.registerCommand('say-hello', () => alert('Hello from Sisyphus plugin!'));
