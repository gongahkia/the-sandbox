"use client"

import type { GameState, SelectedPiece } from "@/lib/gungi-logic"

interface GungiBoardProps {
  gameState: GameState
  onCellClick: (row: number, col: number) => void
  selectedPiece: SelectedPiece | null
}

export default function GungiBoard({ gameState, onCellClick, selectedPiece }: GungiBoardProps) {
  // Determine if a cell is highlighted (selected or potential move)
  const isCellHighlighted = (row: number, col: number) => {
    if (!selectedPiece) return false

    if (selectedPiece.fromBoard && selectedPiece.position) {
      return selectedPiece.position.row === row && selectedPiece.position.col === col
    }

    return false
  }

  // Get the top piece at a position for display
  const getTopPiece = (row: number, col: number) => {
    const stack = gameState.board[row][col]
    return stack.length > 0 ? stack[stack.length - 1] : null
  }

  // Render the board
  return (
    <div className="relative">
      <div className="grid grid-cols-9 gap-0 border-2 border-amber-800 bg-amber-100">
        {Array.from({ length: 9 }, (_, row) =>
          Array.from({ length: 9 }, (_, col) => {
            const topPiece = getTopPiece(row, col)
            const isHighlighted = isCellHighlighted(row, col)
            const stack = gameState.board[row][col]

            return (
              <div
                key={`${row}-${col}`}
                className={`
                  w-10 h-10 md:w-14 md:h-14 border border-amber-700
                  flex items-center justify-center cursor-pointer
                  ${isHighlighted ? "bg-yellow-200" : ""}
                  ${(row + col) % 2 === 0 ? "bg-amber-50" : "bg-amber-100"}
                  hover:bg-amber-200
                `}
                onClick={() => onCellClick(row, col)}
              >
                {topPiece && (
                  <div
                    className={`
                      w-8 h-8 md:w-12 md:h-12 rounded-full flex items-center justify-center
                      font-bold text-xs md:text-sm
                      ${topPiece.player === "black" ? "bg-gray-800 text-white" : "bg-white text-gray-800 border border-gray-400"}
                    `}
                  >
                    {getPieceSymbol(topPiece.type)}
                    {stack.length > 1 && (
                      <span className="absolute top-0 right-0 text-xs bg-red-500 text-white rounded-full w-4 h-4 flex items-center justify-center">
                        {stack.length}
                      </span>
                    )}
                  </div>
                )}
              </div>
            )
          }),
        )}
      </div>

      {/* Board coordinates */}
      <div className="absolute -left-6 top-0 h-full flex flex-col justify-around">
        {Array.from({ length: 9 }, (_, i) => (
          <div key={i} className="text-xs text-center">
            {9 - i}
          </div>
        ))}
      </div>
      <div className="absolute top-full left-0 w-full flex justify-around">
        {Array.from({ length: 9 }, (_, i) => (
          <div key={i} className="text-xs text-center">
            {String.fromCharCode(65 + i)}
          </div>
        ))}
      </div>
    </div>
  )
}

// Helper function to get a symbol for each piece type
function getPieceSymbol(pieceType: string): string {
  const symbols: Record<string, string> = {
    marshal: "M",
    general: "G",
    samurai: "S",
    spy: "Sp",
    cannon: "C",
    fortress: "F",
    archer: "A",
    knight: "K",
    pawn: "P",
  }

  return symbols[pieceType] || "?"
}
