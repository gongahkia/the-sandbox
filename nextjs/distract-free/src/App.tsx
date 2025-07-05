import React from "react";
import Editor from "./components/Editor";
import DistractionCounter from "./components/DistractionCounter";
import Notification from "./components/Notification";
import ThemeToggle from "./components/ThemeToggle";
import { DistractionProvider } from "./context/DistractionContext";

const App: React.FC = () => (
  <DistractionProvider>
    <div className="min-h-screen flex flex-col items-center justify-center p-4">
      <div className="w-full max-w-2xl">
        <div className="flex items-center mb-4">
          <DistractionCounter />
          <ThemeToggle />
        </div>
        <Notification />
        <Editor />
      </div>
    </div>
  </DistractionProvider>
);

export default App;