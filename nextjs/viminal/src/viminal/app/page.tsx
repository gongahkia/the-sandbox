"use client"

import { useState, useEffect } from "react"
import VimCodeEditor from "../components/VimCodeEditor"

export default function Home() {
  const [isSupported, setIsSupported] = useState(false)

  useEffect(() => {
    setIsSupported("showOpenFilePicker" in window)
  }, [])

  if (!isSupported) {
    return (
      <main className="h-screen flex items-center justify-center bg-gray-100">
        <div className="text-center p-8 bg-white rounded-lg shadow-md">
          <h1 className="text-2xl font-bold mb-4">Unsupported Browser</h1>
          <p>Sorry, your browser doesn't support the File System Access API.</p>
          <p className="mt-2">Please use a modern browser like Chrome or Edge.</p>
        </div>
      </main>
    )
  }

  return (
    <main className="h-screen">
      <VimCodeEditor />
    </main>
  )
}