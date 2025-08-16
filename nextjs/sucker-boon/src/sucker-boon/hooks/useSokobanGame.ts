"use client"

import { useState, useCallback, useEffect } from "react"
import type { GameState } from "../types/sokoban"
import { levels } from "../data/levels"

const createInitialState = (levelIndex: number): GameState => ({
  level: levelIndex,
  board: levels[levelIndex].board,
  playerPosition: levels[levelIndex].playerPosition,
  boxPositions: levels[levelIndex].boxPositions,
  targetPositions: levels[levelIndex].targetPositions,
  moves: 0,
  history: [],
})

export const useSokobanGame = () => {
  const [gameState, setGameState] = useState<GameState>(createInitialState(0))

  const movePlayer = useCallback((direction: "up" | "down" | "left" | "right") => {
    setGameState((prevState) => {
      const newState = { ...prevState }
      const { playerPosition, boxPositions, board } = newState

      let newRow = playerPosition.row
      let newCol = playerPosition.col

      switch (direction) {
        case "up":
          newRow--
          break
        case "down":
          newRow++
          break
        case "left":
          newCol--
          break
        case "right":
          newCol++
          break
      }

      if (board[newRow][newCol] === "wall") {
        return prevState
      }

      const boxIndex = boxPositions.findIndex((box) => box.row === newRow && box.col === newCol)

      if (boxIndex !== -1) {
        const newBoxRow = newRow + (newRow - playerPosition.row)
        const newBoxCol = newCol + (newCol - playerPosition.col)

        if (
          board[newBoxRow][newBoxCol] === "wall" ||
          boxPositions.some((box) => box.row === newBoxRow && box.col === newBoxCol)
        ) {
          return prevState
        }

        newState.boxPositions = [
          ...boxPositions.slice(0, boxIndex),
          { row: newBoxRow, col: newBoxCol },
          ...boxPositions.slice(boxIndex + 1),
        ]
      }

      newState.playerPosition = { row: newRow, col: newCol }
      newState.moves++
      newState.history.push(prevState)

      return newState
    })
  }, [])

  const undoMove = useCallback(() => {
    setGameState((prevState) => {
      if (prevState.history.length === 0) return prevState
      const newState = prevState.history[prevState.history.length - 1]
      return {
        ...newState,
        history: newState.history.slice(0, -1),
      }
    })
  }, [])

  const resetLevel = useCallback(() => {
    setGameState(createInitialState(gameState.level))
  }, [gameState.level])

  const changeLevel = useCallback((levelIndex: number) => {
    if (levelIndex >= 0 && levelIndex < levels.length) {
      setGameState(createInitialState(levelIndex))
    }
  }, [])

  const isLevelComplete = useCallback(() => {
    return gameState.boxPositions.every((box) =>
      gameState.targetPositions.some((target) => target.row === box.row && target.col === box.col),
    )
  }, [gameState.boxPositions, gameState.targetPositions])

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      switch (event.key) {
        case "ArrowUp":
          movePlayer("up")
          break
        case "ArrowDown":
          movePlayer("down")
          break
        case "ArrowLeft":
          movePlayer("left")
          break
        case "ArrowRight":
          movePlayer("right")
          break
        case "r":
        case "R":
          resetLevel()
          break
        case "u":
        case "U":
          undoMove()
          break
      }
    }

    window.addEventListener("keydown", handleKeyDown)
    return () => {
      window.removeEventListener("keydown", handleKeyDown)
    }
  }, [movePlayer, resetLevel, undoMove])

  const nextLevel = useCallback(() => {
    if (isLevelComplete() && gameState.level < levels.length - 1) {
      changeLevel(gameState.level + 1)
    }
  }, [isLevelComplete, gameState.level, changeLevel])

  return {
    gameState,
    movePlayer,
    undoMove,
    resetLevel,
    changeLevel,
    isLevelComplete,
    nextLevel,
  }
}