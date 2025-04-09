"use client"

import type React from "react"
import { useEffect, useState } from "react"
import { useSokobanGame } from "../hooks/useSokobanGame"
import type { CellType } from "../types/sokoban"
import { levels } from "../data/levels"

const cellStyles: Record<CellType, string> = {
  wall: "bg-gray-800",
  floor: "bg-gray-200",
  target: "bg-red-300",
  player: "bg-blue-500",
  box: "bg-yellow-600",
  boxOnTarget: "bg-green-500",
}

export const SokobanGame: React.FC = () => {
  const { gameState, undoMove, resetLevel, changeLevel, isLevelComplete, nextLevel } = useSokobanGame()
  const [showNextLevelButton, setShowNextLevelButton] = useState(false)

  useEffect(() => {
    setShowNextLevelButton(isLevelComplete())
  }, [isLevelComplete])

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.key === "Enter" && isLevelComplete()) {
        nextLevel()
      }
    }

    window.addEventListener("keydown", handleKeyDown)
    return () => {
      window.removeEventListener("keydown", handleKeyDown)
    }
  }, [isLevelComplete, nextLevel])

  const renderCell = (cell: CellType, row: number, col: number) => {
    const isPlayer = gameState.playerPosition.row === row && gameState.playerPosition.col === col
    const isBox = gameState.boxPositions.some((box) => box.row === row && box.col === col)
    const isTarget = gameState.targetPositions.some((target) => target.row === row && target.col === col)

    let cellType: CellType = cell

    if (isPlayer) {
      cellType = "player"
    } else if (isBox) {
      cellType = isTarget ? "boxOnTarget" : "box"
    } else if (isTarget) {
      cellType = "target"
    }

    return <div key={`${row}-${col}`} className={`w-8 h-8 border border-gray-400 ${cellStyles[cellType]}`} />
  }

  return (
    <div className="flex justify-center items-start gap-8 w-full max-w-7xl mx-auto">
      <div className="flex-shrink-0 overflow-auto max-h-[80vh]">
        <div className="grid gap-0.5 p-2 bg-gray-300 rounded">
          {gameState.board.map((row, rowIndex) => (
            <div key={rowIndex} className="flex">
              {row.map((cell, colIndex) => renderCell(cell, rowIndex, colIndex))}
            </div>
          ))}
        </div>
      </div>
      <div className="flex flex-col items-start space-y-4">
        <h1 className="text-4xl font-bold">Suckerboon</h1>
        <div>
          <span className="font-bold">Level:</span> {gameState.level + 1} / {levels.length}
        </div>
        <div>
          <span className="font-bold">Moves:</span> {gameState.moves}
        </div>
        <div className="space-y-2">
          <button className="w-full px-4 py-2 bg-yellow-500 text-white rounded" onClick={undoMove}>
            Undo (U)
          </button>
          <button className="w-full px-4 py-2 bg-red-500 text-white rounded" onClick={resetLevel}>
            Reset (R)
          </button>
        </div>
        <div className="space-y-2">
          {gameState.level > 0 && (
            <button
              className="w-full px-4 py-2 bg-green-500 text-white rounded"
              onClick={() => changeLevel(gameState.level - 1)}
            >
              Previous Level
            </button>
          )}
          {showNextLevelButton && gameState.level < levels.length - 1 && (
            <button
              className="w-full px-4 py-2 bg-green-500 text-white rounded"
              onClick={() => changeLevel(gameState.level + 1)}
            >
              Next Level
            </button>
          )}
        </div>
        {isLevelComplete() && (
          <div className="text-2xl font-bold text-green-600">Level Complete! Press Enter for next level.</div>
        )}
        <div className="text-sm text-gray-600">
          Welcome to Suckerboon! Use arrow keys to move, 'U' to undo, 'R' to reset, and Enter for next level when
          complete.
        </div>
      </div>
    </div>
  )
}