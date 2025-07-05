import React from 'react'

export type OutlineEntry = {
  id: string
  text: string
  level: number
  position: number
}

type OutlineSidebarProps = {
  outline: OutlineEntry[]
  onNavigate: (position: number) => void
  onRename: (id: string, newText: string) => void
}

const OutlineSidebar: React.FC<OutlineSidebarProps> = ({ outline, onNavigate, onRename }) => (
  <aside className="w-64 bg-gray-100 dark:bg-gray-800 p-4 overflow-y-auto h-full border-r">
    <h2 className="font-bold mb-4">Outline</h2>
    <ul>
      {outline.map(entry => (
        <li
          key={entry.id}
          className={`pl-${entry.level * 4} cursor-pointer hover:bg-primary/10 rounded transition`}
          onClick={() => onNavigate(entry.position)}
        >
          <input
            className="bg-transparent border-none outline-none w-full"
            value={entry.text}
            onChange={e => onRename(entry.id, e.target.value)}
          />
        </li>
      ))}
    </ul>
  </aside>
)

export default OutlineSidebar