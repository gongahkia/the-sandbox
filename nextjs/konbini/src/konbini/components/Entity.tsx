import type { Position } from "../types"

export type EntityType = "yokai" | "customer" | "obake" | "kappa" | "tengu"

interface EntityState {
  type: EntityType
  position: Position
  direction: number
  isDisguised: boolean
}

export const Entity = {
  draw: (ctx: CanvasRenderingContext2D, entity: EntityState, cellSize: number, lightsOn: boolean) => {
    const { position, type, isDisguised } = entity
    let color = "red"

    if (isDisguised) {
      color = "blue" // Use blue for disguised entities (customers)
    } else {
      switch (type) {
        case "yokai":
          color = "purple"
          break
        case "obake":
          color = "green"
          break
        case "kappa":
          color = "teal"
          break
        case "tengu":
          color = "maroon"
          break
      }
    }

    ctx.fillStyle = lightsOn ? color : `dark${color}`
    ctx.fillRect(position.x * cellSize, position.y * cellSize, cellSize, cellSize)

    // Draw eyes
    ctx.fillStyle = "white"
    ctx.fillRect(
      position.x * cellSize + cellSize * 0.2,
      position.y * cellSize + cellSize * 0.2,
      cellSize * 0.2,
      cellSize * 0.2,
    )
    ctx.fillRect(
      position.x * cellSize + cellSize * 0.6,
      position.y * cellSize + cellSize * 0.2,
      cellSize * 0.2,
      cellSize * 0.2,
    )
  },
  checkCollision: (entityPosition: Position, playerPosition: Position): boolean => {
    return entityPosition.x === playerPosition.x && entityPosition.y === playerPosition.y
  },
}

export const createEntity = (type: EntityType, position: Position): EntityState => ({
  type,
  position,
  direction: 0,
  isDisguised: false,
})