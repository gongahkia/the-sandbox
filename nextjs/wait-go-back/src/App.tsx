import React, { useState, useCallback } from 'react'
import Editor from './components/Editor'
import OutlineSidebar, { OutlineEntry } from './components/OutlineSidebar'

const App: React.FC = () => {
  const [outline, setOutline] = useState<OutlineEntry[]>([])

  const handleOutlineChange = useCallback((newOutline: OutlineEntry[]) => {
    setOutline(newOutline)
  }, [])

  const handleNavigate = (position: number) => {
    // Implement scroll-to-position logic using Tiptap's API
  }

  const handleRename = (id: string, newText: string) => {
    // Implement renaming logic (update outline state and optionally the editor)
  }

  return (
    <div className="flex h-screen bg-light dark:bg-dark">
      <OutlineSidebar
        outline={outline}
        onNavigate={handleNavigate}
        onRename={handleRename}
      />
      <main className="flex-1 p-8 overflow-y-auto">
        <Editor onOutlineChange={handleOutlineChange} />
      </main>
    </div>
  )
}

export default App