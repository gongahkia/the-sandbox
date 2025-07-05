import React, { useContext, useRef } from "react";
import { LexicalComposer } from "@lexical/react/LexicalComposer";
import { RichTextPlugin } from "@lexical/react/LexicalRichTextPlugin";
import { ContentEditable } from "@lexical/react/LexicalContentEditable";
import { HistoryPlugin } from "@lexical/react/LexicalHistoryPlugin";
import { LexicalErrorBoundary } from "@lexical/react/LexicalErrorBoundary";
import { OnChangePlugin } from "@lexical/react/LexicalOnChangePlugin";
import { DistractionContext } from "../context/DistractionContext";
import useFocusTracker from "../hooks/useFocusTracker";
import useTypingPause from "../hooks/useTypingPause";
import autosave from "../utils/autosave";

const theme = {
  paragraph: "mb-2",
  // Add more Tailwind classes as needed
};

const Editor: React.FC = () => {
  const { incrementDistraction, resetPause, setPauseDuration } = useContext(DistractionContext);
  const editorRef = useRef<HTMLDivElement>(null);

  useFocusTracker(incrementDistraction);
  useTypingPause(resetPause, setPauseDuration, incrementDistraction);

  const initialConfig = {
    namespace: "DistractionEditor",
    theme,
    onError: (error: Error) => { throw error; },
  };

  const handleChange = (editorState: any) => {
    // Autosave logic
    editorState.read(() => {
      const content = JSON.stringify(editorState.toJSON());
      autosave(content);
    });
    resetPause();
  };

  return (
    <div className="bg-white dark:bg-gray-900 rounded-lg shadow-md p-4 min-h-[400px]">
      <LexicalComposer initialConfig={initialConfig}>
        <RichTextPlugin
          contentEditable={
            <ContentEditable
              className="outline-none min-h-[300px] text-lg bg-transparent"
              ref={editorRef}
              aria-placeholder="Start writing..."
            />
          }
          placeholder={<div className="text-gray-400">Start writing...</div>}
          ErrorBoundary={LexicalErrorBoundary}
        />
        <HistoryPlugin />
        <OnChangePlugin onChange={handleChange} />
      </LexicalComposer>
    </div>
  );
};

export default Editor;