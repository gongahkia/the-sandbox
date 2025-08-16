"use client"

import React, { useState, useEffect, useRef } from "react"
import CodeMirror from "@uiw/react-codemirror"
import { javascript } from "@codemirror/lang-javascript"
import { vim } from "@replit/codemirror-vim"
import { EditorView } from "@codemirror/view"

export default function VimCodeEditor() {
  const [code, setCode] = useState("")
  const [fileName, setFileName] = useState("")
  const fileHandleRef = useRef<FileSystemFileHandle | null>(null)

  useEffect(() => {
    loadFile()
  }, [])

  const loadFile = async () => {
    try {
      const [fileHandle] = await window.showOpenFilePicker()
      fileHandleRef.current = fileHandle
      const file = await fileHandle.getFile()
      setFileName(file.name)
      const content = await file.text()
      setCode(content)
    } catch (error) {
      console.error("Error loading file:", error)
      setCode("// Failed to load file. You can still edit here.\n")
    }
  }

  const saveFile = async () => {
    if (!fileHandleRef.current) {
      console.error("No file loaded")
      return
    }

    try {
      const writable = await fileHandleRef.current.createWritable()
      await writable.write(code)
      await writable.close()
      console.log("File saved successfully")
    } catch (error) {
      console.error("Error saving file:", error)
    }
  }

  const defineCustomCommands = (cm: CodeMirror.Editor) => {
    cm.defineEx("wq", "wq", () => {
      saveFile().then(() => {
        console.log("File saved. You can close the tab now.")
      })
    })
  }

  const onChange = React.useCallback((value: string) => {
    setCode(value)
  }, [])

  return (
    <div className="w-full h-screen flex flex-col">
      <div className="bg-gray-800 text-white p-2">{fileName ? `Editing: ${fileName}` : "No file loaded"}</div>
      <div className="flex-grow">
        <CodeMirror
          value={code}
          height="100%"
          extensions={[
            javascript(),
            vim({
              status: true,
              cmdLine: true,
            }),
            EditorView.updateListener.of((update) => {
              if (update.docChanged) {
                const cm = update.view.state.field(vim()).cm
                if (cm) {
                  defineCustomCommands(cm)
                }
              }
            }),
            EditorView.lineWrapping,
          ]}
          onChange={onChange}
          theme="dark"
        />
      </div>
    </div>
  )
}