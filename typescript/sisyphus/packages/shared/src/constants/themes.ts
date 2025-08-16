export const THEMES = ['light', 'dark', 'high-contrast'] as const;
export type ThemeType = typeof THEMES[number];
