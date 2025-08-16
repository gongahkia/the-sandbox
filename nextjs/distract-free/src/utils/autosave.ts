const AUTOSAVE_KEY = "editor-content";

const autosave = (content: string) => {
  localStorage.setItem(AUTOSAVE_KEY, content);
};

export const loadAutosave = (): string | null => {
  return localStorage.getItem(AUTOSAVE_KEY);
};

export default autosave;