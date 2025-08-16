import React, { useRef, useEffect } from 'react';
import { EditorView, basicSetup } from 'codemirror';
import { oneDark } from '@codemirror/theme-one-dark';
import { javascript } from '@codemirror/lang-javascript';

export interface EditorProps {
  value: string;
  language?: string;
  theme?: string;
  onChange?: (val: string) => void;
}

export default function Editor({
  value = '',
  language = 'javascript',
  theme = 'dark',
  onChange,
}: EditorProps) {
  const editorRef = useRef<HTMLDivElement>(null);
  const viewRef = useRef<EditorView | null>(null);

  useEffect(() => {
    if (editorRef.current && !viewRef.current) {
      viewRef.current = new EditorView({
        doc: value,
        extensions: [
          basicSetup,
          javascript(),
          theme === 'dark' ? oneDark : [],
          EditorView.updateListener.of((v) => {
            if (v.docChanged && onChange)
              onChange(v.state.doc.toString());
          }),
        ],
        parent: editorRef.current,
      });
    }
    return () => {
      viewRef.current?.destroy();
      viewRef.current = null;
    };
  }, [editorRef, theme, language]);

  useEffect(() => {
    viewRef.current?.dispatch({
      changes: { from: 0, to: viewRef.current.state.doc.length, insert: value },
    });
  }, [value]);

  return <div ref={editorRef} style={{ height: '100%', fontSize: '1em' }} />;
}