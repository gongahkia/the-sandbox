export const baseThemes = {
  dark: {
    background: '#1a1a1a',
    foreground: '#f5f5f5',
    cursor: '#ffa500',
  },
  light: {
    background: '#fff',
    foreground: '#222',
    cursor: '#2e8b57',
  },
};

export function useTerminalTheme(theme: string) {
  return baseThemes[theme] || baseThemes.dark;
}