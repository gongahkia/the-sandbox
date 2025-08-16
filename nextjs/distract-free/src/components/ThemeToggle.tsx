import React from "react";

const ThemeToggle: React.FC = () => {
  const [dark, setDark] = React.useState(() =>
    window.matchMedia("(prefers-color-scheme: dark)").matches
  );

  React.useEffect(() => {
    document.documentElement.classList.toggle("dark", dark);
  }, [dark]);

  return (
    <button
      className="ml-auto px-3 py-1 rounded bg-gray-200 dark:bg-gray-700"
      onClick={() => setDark((d) => !d)}
      aria-label="Toggle dark mode"
    >
      {dark ? "🌙" : "☀️"}
    </button>
  );
};

export default ThemeToggle;