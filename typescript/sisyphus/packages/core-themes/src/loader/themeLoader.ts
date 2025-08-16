import darkTheme from '../themes-json/dark.json';
import lightTheme from '../themes-json/light.json';

export const THEMES = {
  dark: darkTheme,
  light: lightTheme,
};

export function applyTheme(theme: string) {
  const themeDef = THEMES[theme] || THEMES['dark'];
  Object.entries(themeDef.colors).forEach(([key, val]) => {
    document.documentElement.style.setProperty(`--color-${key.toLowerCase()}`, val);
  });
  document.documentElement.style.setProperty('--app-font', themeDef.font);
  document.body.setAttribute('data-theme', themeDef.type);
}
