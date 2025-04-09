export type CellType = "wall" | "floor" | "target" | "player" | "box" | "boxOnTarget"

export type Position = {
  row: number
  col: number
}

export type GameState = {
  level: number
  board: CellType[][]
  playerPosition: Position
  boxPositions: Position[]
  targetPositions: Position[]
  moves: number
  history: GameState[]
}

export type Level = {
  board: CellType[][]
  playerPosition: Position
  boxPositions: Position[]
  targetPositions: Position[]
}