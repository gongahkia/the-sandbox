import { useState } from 'react';

// Simple editor state hook (for React Context API, extensible)
export const useEditorStore = () => {
  const [doc, setDoc] = useState<string>('');
  const [language, setLanguage] = useState<string>('javascript');
  const [theme, setTheme] = useState<string>('dark');
  // Add selection/cursor, etc
  return {
    doc,
    setDoc,
    language,
    setLanguage,
    theme,
    setTheme,
  };
};