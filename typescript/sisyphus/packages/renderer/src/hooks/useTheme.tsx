import React from 'react';

const ThemeContext = React.createContext({
  theme: 'light',
  setTheme: (_: string) => {},
});

export const ThemeProvider = ({ children }: React.PropsWithChildren<{}>) => {
  const [theme, setTheme] = React.useState('light');
  React.useEffect(() => {
    document.body.dataset.theme = theme; // For CSS vars
  }, [theme]);
  return <ThemeContext.Provider value={{ theme, setTheme }}>{children}</ThemeContext.Provider>;
};

export function useTheme() {
  return React.useContext(ThemeContext);
}