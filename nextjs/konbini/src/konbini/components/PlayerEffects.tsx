import type { Position } from "../types"

export interface PlayerEffects {
  hasMigraine: boolean
  isHallucinating: boolean
}

export const PlayerEffects = {
  draw: (ctx: CanvasRenderingContext2D, effects: PlayerEffects, playerPosition: Position, cellSize: number) => {
    if (effects.hasMigraine) {
      ctx.fillStyle = "rgba(255, 0, 0, 0.2)"
      ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height)
    }

    if (effects.isHallucinating) {
      // Draw some random shapes
      for (let i = 0; i < 10; i++) {
        ctx.fillStyle = `rgba(${Math.random() * 255}, ${Math.random() * 255}, ${Math.random() * 255}, 0.5)`
        ctx.beginPath()
        ctx.arc(Math.random() * ctx.canvas.width, Math.random() * ctx.canvas.height, Math.random() * 20, 0, 2 * Math.PI)
        ctx.fill()
      }
    }
  },
}