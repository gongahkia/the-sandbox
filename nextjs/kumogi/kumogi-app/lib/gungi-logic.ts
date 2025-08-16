// Types
export type PieceType = "marshal" | "general" | "samurai" | "spy" | "cannon" | "fortress" | "archer" | "knight" | "pawn"
export type Player = "black" | "white"

export interface Piece {
  type: PieceType
  player: Player
}

export interface Position {
  row: number
  col: number
}

export interface SelectedPiece {
  pieceType: PieceType
  position?: Position
  fromBoard: boolean
}

export interface GameState {
  board: Piece[][][] // 3D array: row, column, stack of pieces
  currentPlayer: Player
  capturedPieces: {
    black: Piece[]
    white: Piece[]
  }
}

// Create initial game state
export function createInitialGameState(): GameState {
  // Create an empty 9x9 board
  const board: Piece[][][] = Array(9)
    .fill(null)
    .map(() =>
      Array(9)
        .fill(null)
        .map(() => []),
    )

  return {
    board,
    currentPlayer: "black", // Black goes first
    capturedPieces: {
      black: [],
      white: [],
    },
  }
}

// Check if a piece can be placed at a position
export function canPlacePiece(
  gameState: GameState,
  pieceType: PieceType,
  row: number,
  col: number,
  player: Player,
): boolean {
  // Check if position is within board boundaries
  if (row < 0 || row >= 9 || col < 0 || col >= 9) {
    return false
  }

  // During placement phase, players can only place pieces on their side
  if (player === "black" && row < 3) {
    return false
  }
  if (player === "white" && row > 5) {
    return false
  }

  // Cannot place on top of opponent's piece
  const stack = gameState.board[row][col]
  if (stack.length > 0 && stack[stack.length - 1].player !== player) {
    return false
  }

  // Cannot stack more than 3 pieces
  if (stack.length >= 3) {
    return false
  }

  // Marshal can only be placed once per player
  if (pieceType === "marshal") {
    // Check if player already has a marshal on the board
    for (let r = 0; r < 9; r++) {
      for (let c = 0; c < 9; c++) {
        const pieces = gameState.board[r][c]
        if (pieces.some((p) => p.type === "marshal" && p.player === player)) {
          return false
        }
      }
    }
  }

  return true
}

// Place a piece on the board
export function placePiece(
  gameState: GameState,
  pieceType: PieceType,
  row: number,
  col: number,
  player: Player,
): GameState {
  // Create a deep copy of the game state
  const newGameState = JSON.parse(JSON.stringify(gameState)) as GameState

  // Add the piece to the stack
  newGameState.board[row][col].push({
    type: pieceType,
    player,
  })

  // Switch player
  newGameState.currentPlayer = player === "black" ? "white" : "black"

  return newGameState
}

// Check if a piece can move from one position to another
export function canMovePiece(
  gameState: GameState,
  fromRow: number,
  fromCol: number,
  toRow: number,
  toCol: number,
): boolean {
  // Check if positions are within board boundaries
  if (
    fromRow < 0 ||
    fromRow >= 9 ||
    fromCol < 0 ||
    fromCol >= 9 ||
    toRow < 0 ||
    toRow >= 9 ||
    toCol < 0 ||
    toCol >= 9
  ) {
    return false
  }

  // Check if there's a piece at the source position
  const sourceStack = gameState.board[fromRow][fromCol]
  if (sourceStack.length === 0) {
    return false
  }

  // Check if the top piece belongs to the current player
  const piece = sourceStack[sourceStack.length - 1]
  if (piece.player !== gameState.currentPlayer) {
    return false
  }

  // Check if the destination has less than 3 pieces (stacking limit)
  const destStack = gameState.board[toRow][toCol]
  if (destStack.length >= 3) {
    return false
  }

  // Check if the destination's top piece is not an opponent's piece
  if (destStack.length > 0 && destStack[destStack.length - 1].player !== gameState.currentPlayer) {
    return false
  }

  // Check movement patterns based on piece type
  // This is a simplified version of Gungi movement rules
  const rowDiff = Math.abs(toRow - fromRow)
  const colDiff = Math.abs(toCol - fromCol)

  switch (piece.type) {
    case "marshal":
      // Marshal can move one space in any direction
      return rowDiff <= 1 && colDiff <= 1

    case "general":
      // General can move like a rook (orthogonally) up to 2 spaces
      return (rowDiff === 0 && colDiff <= 2) || (colDiff === 0 && rowDiff <= 2)

    case "samurai":
      // Samurai can move like a bishop (diagonally) up to 2 spaces
      return rowDiff === colDiff && rowDiff <= 2

    case "spy":
      // Spy can move one space in any direction
      return rowDiff <= 1 && colDiff <= 1

    case "cannon":
      // Cannon moves orthogonally up to 3 spaces
      return (rowDiff === 0 && colDiff <= 3) || (colDiff === 0 && rowDiff <= 3)

    case "fortress":
      // Fortress can move one space orthogonally
      return (rowDiff === 1 && colDiff === 0) || (rowDiff === 0 && colDiff === 1)

    case "archer":
      // Archer can move diagonally up to 3 spaces
      return rowDiff === colDiff && rowDiff <= 3

    case "knight":
      // Knight moves in an L-shape (similar to chess)
      return (rowDiff === 2 && colDiff === 1) || (rowDiff === 1 && colDiff === 2)

    case "pawn":
      // Pawn moves one space forward (direction depends on player)
      if (piece.player === "black") {
        return colDiff === 0 && fromRow - toRow === 1
      } else {
        return colDiff === 0 && toRow - fromRow === 1
      }

    default:
      return false
  }
}

// Move a piece on the board
export function movePiece(
  gameState: GameState,
  fromRow: number,
  fromCol: number,
  toRow: number,
  toCol: number,
): GameState {
  // Create a deep copy of the game state
  const newGameState = JSON.parse(JSON.stringify(gameState)) as GameState

  // Get the piece to move
  const sourceStack = newGameState.board[fromRow][fromCol]
  const piece = sourceStack.pop()!

  // Add the piece to the destination
  newGameState.board[toRow][toCol].push(piece)

  // Check for win condition (capturing opponent's marshal)
  const hasBlackMarshal = newGameState.board.some((row) =>
    row.some((stack) => stack.some((p) => p.type === "marshal" && p.player === "black")),
  )

  const hasWhiteMarshal = newGameState.board.some((row) =>
    row.some((stack) => stack.some((p) => p.type === "marshal" && p.player === "white")),
  )

  if (!hasBlackMarshal) {
    console.log("White wins!")
  } else if (!hasWhiteMarshal) {
    console.log("Black wins!")
  }

  // Switch player
  newGameState.currentPlayer = gameState.currentPlayer === "black" ? "white" : "black"

  return newGameState
}