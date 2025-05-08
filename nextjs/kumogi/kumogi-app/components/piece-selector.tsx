"use client"

import { Card } from "@/components/ui/card"
import { Button } from "@/components/ui/button"
import type { GameState, PieceType, Player } from "@/lib/gungi-logic"

interface PieceSelectorProps {
  onSelectPiece: (pieceType: PieceType) => void
  currentPlayer: Player
  gameState: GameState
}

export default function PieceSelector({ onSelectPiece, currentPlayer, gameState }: PieceSelectorProps) {
  // Define the available pieces and their limits
  const pieceLimits: Record<PieceType, number> = {
    marshal: 1,
    general: 2,
    samurai: 2,
    spy: 2,
    cannon: 2,
    fortress: 2,
    archer: 2,
    knight: 2,
    pawn: 6,
  }

  // Count how many of each piece type the current player has placed
  const countPlacedPieces = (pieceType: PieceType): number => {
    let count = 0

    for (let row = 0; row < 9; row++) {
      for (let col = 0; col < 9; col++) {
        const stack = gameState.board[row][col]
        count += stack.filter((p) => p.type === pieceType && p.player === currentPlayer).length
      }
    }

    return count
  }

  return (
    <Card className="p-4">
      <h2 className="text-xl font-bold mb-2">Available Pieces</h2>
      <p className="mb-2">{currentPlayer === "black" ? "Black" : "White"}'s turn</p>

      <div className="grid grid-cols-2 gap-2">
        {Object.entries(pieceLimits).map(([pieceType, limit]) => {
          const placed = countPlacedPieces(pieceType as PieceType)
          const remaining = limit - placed

          return (
            <Button
              key={pieceType}
              variant="outline"
              className={`
                flex justify-between items-center
                ${currentPlayer === "black" ? "border-gray-800" : "border-gray-300"}
                ${remaining <= 0 ? "opacity-50 cursor-not-allowed" : ""}
              `}
              onClick={() => remaining > 0 && onSelectPiece(pieceType as PieceType)}
              disabled={remaining <= 0}
            >
              <span className="font-medium">{pieceType}</span>
              <span className="text-xs bg-gray-200 px-2 py-1 rounded-full">
                {remaining}/{limit}
              </span>
            </Button>
          )
        })}
      </div>
    </Card>
  )
}