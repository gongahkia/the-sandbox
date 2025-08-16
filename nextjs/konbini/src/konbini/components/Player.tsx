export const Player = {
  draw: (ctx: CanvasRenderingContext2D, position: { x: number; y: number }, cellSize: number) => {
    ctx.fillStyle = "blue"
    ctx.fillRect(position.x * cellSize, position.y * cellSize, cellSize, cellSize)
  },
}