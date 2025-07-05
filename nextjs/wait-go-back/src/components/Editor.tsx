import React from 'react'
import { useEditor, EditorContent } from '@tiptap/react'
import StarterKit from '@tiptap/starter-kit'
import { extractOutline } from './OutlineUtils'

type EditorProps = {
  onOutlineChange: (outline: OutlineEntry[]) => void
}

const Editor: React.FC<EditorProps> = ({ onOutlineChange }) => {
  const editor = useEditor({
    extensions: [StarterKit],
    content: '<h1>Start writing...</h1>',
    onUpdate: ({ editor }) => {
      const json = editor.getJSON()
      const outline = extractOutline(json)
      onOutlineChange(outline)
    },
  })

  return (
    <div className="prose dark:prose-invert max-w-none h-full">
      <EditorContent editor={editor} />
    </div>
  )
}

export default Editor