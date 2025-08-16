export interface PluginAPI {
  registerCommand(id: string, handler: () => void): void;
  registerPanel(id: string, component: React.ComponentType): void;
  registerTheme(id: string, theme: object): void;
  registerLSP(id: string, config: object): void;
}

export const pluginAPI: PluginAPI = {
  registerCommand: (id, handler) => {
    // Add command to global command palette/context
  },
  registerPanel: (id, component) => {
    // Dynamically add panel UI to layout
  },
  registerTheme: (id, theme) => {
    // Add theme to theme registry
  },
  registerLSP: (id, config) => {
    // Attach LSP configuration
  }
};