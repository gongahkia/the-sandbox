"use client"

import { useState } from "react"
import { Button } from "@/components/ui/button"
import { Card } from "@/components/ui/card"
import GungiBoard from "./gungi-board"
import PieceSelector from "./piece-selector"
import {
  createInitialGameState,
  canPlacePiece,
  placePiece,
  movePiece,
  canMovePiece,
  type GameState,
  type PieceType,
  type SelectedPiece,
} from "@/lib/gungi-logic"

export default function GungiGame() {
  const [gameState, setGameState] = useState<GameState>(createInitialGameState())
  const [selectedPiece, setSelectedPiece] = useState<SelectedPiece | null>(null)
  const [phase, setPhase] = useState<"placement" | "movement">("placement")
  const [placementTurnsLeft, setPlacementTurnsLeft] = useState(40) // 20 turns per player
  const [message, setMessage] = useState<string>("Black's turn to place a piece")

  // Handle board cell click
  const handleCellClick = (row: number, col: number) => {
    if (phase === "placement") {
      handlePlacementPhase(row, col)
    } else {
      handleMovementPhase(row, col)
    }
  }

  // Handle placement phase logic
  const handlePlacementPhase = (row: number, col: number) => {
    if (!selectedPiece || !selectedPiece.pieceType) {
      setMessage(`Select a piece to place first`)
      return
    }

    if (canPlacePiece(gameState, selectedPiece.pieceType, row, col, gameState.currentPlayer)) {
      const newGameState = placePiece(gameState, selectedPiece.pieceType, row, col, gameState.currentPlayer)

      setGameState(newGameState)
      setSelectedPiece(null)
      setPlacementTurnsLeft((prev) => prev - 1)

      // Check if placement phase is over
      if (placementTurnsLeft <= 1) {
        setPhase("movement")
        setMessage(
          `Placement phase complete. ${newGameState.currentPlayer === "black" ? "Black" : "White"}'s turn to move`,
        )
      } else {
        setMessage(`${newGameState.currentPlayer === "black" ? "Black" : "White"}'s turn to place a piece`)
      }
    } else {
      setMessage(`Cannot place ${selectedPiece.pieceType} at that position`)
    }
  }

  // Handle movement phase logic
  const handleMovementPhase = (row: number, col: number) => {
    // If no piece is selected, try to select one
    if (!selectedPiece) {
      const pieceAtPosition = gameState.board[row][col].find((p) => p.player === gameState.currentPlayer)

      if (pieceAtPosition) {
        setSelectedPiece({
          pieceType: pieceAtPosition.type,
          position: { row, col },
          fromBoard: true,
        })
        setMessage(`Selected ${pieceAtPosition.type}. Click a destination.`)
      } else {
        setMessage(`No ${gameState.currentPlayer} piece at that position`)
      }
      return
    }

    // If a piece is already selected and it's from the board, try to move it
    if (selectedPiece.fromBoard) {
      const fromRow = selectedPiece.position!.row
      const fromCol = selectedPiece.position!.col

      if (canMovePiece(gameState, fromRow, fromCol, row, col)) {
        const newGameState = movePiece(gameState, fromRow, fromCol, row, col)
        setGameState(newGameState)
        setSelectedPiece(null)
        setMessage(`${newGameState.currentPlayer === "black" ? "Black" : "White"}'s turn to move`)
      } else {
        setMessage(`Cannot move to that position`)
      }
    }
  }

  // Handle piece selection from the piece selector
  const handlePieceSelect = (pieceType: PieceType) => {
    setSelectedPiece({
      pieceType,
      fromBoard: false,
    })
    setMessage(`Selected ${pieceType}. Click on the board to place it.`)
  }

  // Reset the game
  const resetGame = () => {
    setGameState(createInitialGameState())
    setSelectedPiece(null)
    setPhase("placement")
    setPlacementTurnsLeft(40)
    setMessage("Black's turn to place a piece")
  }

  return (
    <div className="flex flex-col items-center gap-6">
      <div className="flex flex-col md:flex-row gap-8 items-center md:items-start">
        <div className="order-2 md:order-1">
          <GungiBoard gameState={gameState} onCellClick={handleCellClick} selectedPiece={selectedPiece} />
        </div>

        <div className="order-1 md:order-2 flex flex-col gap-4">
          <Card className="p-4">
            <h2 className="text-xl font-bold mb-2">Game Info</h2>
            <p className="mb-2">Phase: {phase === "placement" ? "Placement" : "Movement"}</p>
            {phase === "placement" && <p className="mb-2">Placement turns left: {placementTurnsLeft}</p>}
            <p className="mb-2">Current Player: {gameState.currentPlayer === "black" ? "Black" : "White"}</p>
            <p className="mb-2 font-medium">{message}</p>
            <Button onClick={resetGame} className="mt-2">
              Reset Game
            </Button>
          </Card>

          {phase === "placement" && (
            <PieceSelector
              onSelectPiece={handlePieceSelect}
              currentPlayer={gameState.currentPlayer}
              gameState={gameState}
            />
          )}
        </div>
      </div>

      <Card className="p-4 max-w-2xl">
        <h2 className="text-xl font-bold mb-2">How to Play</h2>
        <p className="mb-2">Gungi is a strategic board game from Hunter x Hunter. This is a simplified version.</p>
        <ol className="list-decimal pl-5 space-y-1">
          <li>The game has two phases: Placement and Movement</li>
          <li>During Placement, players take turns placing pieces on their side of the board</li>
          <li>After all pieces are placed, the Movement phase begins</li>
          <li>During Movement, players take turns moving their pieces according to their movement patterns</li>
          <li>The goal is to capture the opponent's Marshal (similar to a king in chess)</li>
        </ol>
      </Card>
    </div>
  )
}