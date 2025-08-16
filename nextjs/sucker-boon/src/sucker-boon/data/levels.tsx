import type { Level } from "../types/sokoban"

export const levels: Level[] = [
  // Level 1
  {
    board: [
      ["wall", "wall", "wall", "wall", "wall"],
      ["wall", "floor", "floor", "floor", "wall"],
      ["wall", "floor", "floor", "floor", "wall"],
      ["wall", "floor", "floor", "floor", "wall"],
      ["wall", "wall", "wall", "wall", "wall"],
    ],
    playerPosition: { row: 1, col: 1 },
    boxPositions: [{ row: 2, col: 2 }],
    targetPositions: [{ row: 3, col: 3 }],
  },
  // Level 2
  {
    board: [
      ["wall", "wall", "wall", "wall", "wall", "wall"],
      ["wall", "floor", "floor", "floor", "floor", "wall"],
      ["wall", "floor", "floor", "floor", "floor", "wall"],
      ["wall", "floor", "floor", "floor", "floor", "wall"],
      ["wall", "floor", "floor", "floor", "floor", "wall"],
      ["wall", "wall", "wall", "wall", "wall", "wall"],
    ],
    playerPosition: { row: 1, col: 1 },
    boxPositions: [
      { row: 2, col: 2 },
      { row: 3, col: 3 },
    ],
    targetPositions: [
      { row: 4, col: 4 },
      { row: 4, col: 1 },
    ],
  },
  // Level 3
  {
    board: [
      ["wall", "wall", "wall", "wall", "wall", "wall", "wall"],
      ["wall", "floor", "floor", "floor", "floor", "floor", "wall"],
      ["wall", "floor", "wall", "floor", "wall", "floor", "wall"],
      ["wall", "floor", "floor", "floor", "floor", "floor", "wall"],
      ["wall", "wall", "wall", "wall", "wall", "wall", "wall"],
    ],
    playerPosition: { row: 1, col: 1 },
    boxPositions: [
      { row: 2, col: 2 },
      { row: 2, col: 4 },
    ],
    targetPositions: [
      { row: 3, col: 1 },
      { row: 3, col: 5 },
    ],
  },
  // Level 4
  {
    board: [
      ["wall", "wall", "wall", "wall", "wall", "wall"],
      ["wall", "floor", "floor", "floor", "floor", "wall"],
      ["wall", "floor", "wall", "wall", "floor", "wall"],
      ["wall", "floor", "floor", "floor", "floor", "wall"],
      ["wall", "floor", "wall", "wall", "floor", "wall"],
      ["wall", "floor", "floor", "floor", "floor", "wall"],
      ["wall", "wall", "wall", "wall", "wall", "wall"],
    ],
    playerPosition: { row: 1, col: 1 },
    boxPositions: [
      { row: 2, col: 2 },
      { row: 3, col: 3 },
      { row: 4, col: 4 },
    ],
    targetPositions: [
      { row: 5, col: 1 },
      { row: 5, col: 2 },
      { row: 5, col: 3 },
    ],
  },
  // Level 5
  {
    board: [
      ["wall", "wall", "wall", "wall", "wall", "wall", "wall"],
      ["wall", "floor", "floor", "floor", "floor", "floor", "wall"],
      ["wall", "floor", "wall", "floor", "wall", "floor", "wall"],
      ["wall", "floor", "wall", "floor", "wall", "floor", "wall"],
      ["wall", "floor", "floor", "floor", "floor", "floor", "wall"],
      ["wall", "wall", "wall", "wall", "wall", "wall", "wall"],
    ],
    playerPosition: { row: 1, col: 1 },
    boxPositions: [
      { row: 2, col: 2 },
      { row: 2, col: 4 },
      { row: 3, col: 3 },
    ],
    targetPositions: [
      { row: 4, col: 1 },
      { row: 4, col: 3 },
      { row: 4, col: 5 },
    ],
  },
  // ... (continuing with more levels)
  // Level 50
  {
    board: [
      ["wall", "wall", "wall", "wall", "wall", "wall", "wall", "wall", "wall"],
      ["wall", "floor", "floor", "floor", "wall", "floor", "floor", "floor", "wall"],
      ["wall", "floor", "wall", "floor", "wall", "floor", "wall", "floor", "wall"],
      ["wall", "floor", "wall", "floor", "floor", "floor", "wall", "floor", "wall"],
      ["wall", "floor", "wall", "wall", "wall", "wall", "wall", "floor", "wall"],
      ["wall", "floor", "floor", "floor", "floor", "floor", "floor", "floor", "wall"],
      ["wall", "wall", "wall", "wall", "wall", "wall", "wall", "wall", "wall"],
    ],
    playerPosition: { row: 1, col: 1 },
    boxPositions: [
      { row: 2, col: 2 },
      { row: 2, col: 6 },
      { row: 3, col: 4 },
      { row: 4, col: 7 },
      { row: 5, col: 3 },
    ],
    targetPositions: [
      { row: 5, col: 1 },
      { row: 5, col: 3 },
      { row: 5, col: 5 },
      { row: 5, col: 7 },
      { row: 3, col: 1 },
    ],
  },
]

// Add more levels here...
// We'll generate 45 more levels programmatically to reach a total of 50

type CellType = "wall" | "floor"
type Position = { row: number; col: number }

function generateLevel(levelNumber: number): Level {
  const width = 7 + (levelNumber % 4)
  const height = 7 + (levelNumber % 3)
  const boxCount = 2 + Math.floor(levelNumber / 10)

  const board: CellType[][] = Array(height)
    .fill(0)
    .map(() => Array(width).fill("floor"))

  // Add walls
  for (let i = 0; i < height; i++) {
    board[i][0] = "wall"
    board[i][width - 1] = "wall"
  }
  for (let j = 0; j < width; j++) {
    board[0][j] = "wall"
    board[height - 1][j] = "wall"
  }

  // Add some random internal walls
  for (let i = 0; i < boxCount; i++) {
    const row = 1 + Math.floor(Math.random() * (height - 2))
    const col = 1 + Math.floor(Math.random() * (width - 2))
    board[row][col] = "wall"
  }

  const boxPositions: Position[] = []
  const targetPositions: Position[] = []

  // Add boxes and targets
  for (let i = 0; i < boxCount; i++) {
    let row, col
    do {
      row = 1 + Math.floor(Math.random() * (height - 2))
      col = 1 + Math.floor(Math.random() * (width - 2))
    } while (board[row][col] !== "floor" || boxPositions.some((pos) => pos.row === row && pos.col === col))
    boxPositions.push({ row, col })

    do {
      row = 1 + Math.floor(Math.random() * (height - 2))
      col = 1 + Math.floor(Math.random() * (width - 2))
    } while (board[row][col] !== "floor" || targetPositions.some((pos) => pos.row === row && pos.col === col))
    targetPositions.push({ row, col })
  }

  // Set player position
  let playerRow, playerCol
  do {
    playerRow = 1 + Math.floor(Math.random() * (height - 2))
    playerCol = 1 + Math.floor(Math.random() * (width - 2))
  } while (
    board[playerRow][playerCol] !== "floor" ||
    boxPositions.some((pos) => pos.row === playerRow && pos.col === playerCol)
  )

  return {
    board,
    playerPosition: { row: playerRow, col: playerCol },
    boxPositions,
    targetPositions,
  }
}

for (let i = 6; i <= 50; i++) {
  levels.push(generateLevel(i))
}