import { pluginAPI } from '@sisyphus/core-plugins/src/api/pluginApi';

const solarizedTheme = {
  name: "Solarized Light",
  type: "light",
  colors: {
    background: "#fdf6e3",
    foreground: "#657b83",
    primary: "#b58900",
    secondary: "#2aa198",
    accent: "#cb4b16",
    editorBg: "#eee8d5",
    editorFg: "#586e75"
  },
  font: "Fira Code, monospace"
};

// Register additional theme at runtime
pluginAPI.registerTheme('solarized-light', solarizedTheme);
