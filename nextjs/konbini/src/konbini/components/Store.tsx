export const Store = {
  shelves: [
    // Horizontal shelves
    { x: 2, y: 2, width: 2, height: 1 },
    { x: 6, y: 2, width: 2, height: 1 },
    { x: 10, y: 2, width: 2, height: 1 },
    { x: 14, y: 2, width: 2, height: 1 },
    { x: 2, y: 6, width: 2, height: 1 },
    { x: 6, y: 6, width: 2, height: 1 },
    { x: 10, y: 6, width: 2, height: 1 },
    { x: 14, y: 6, width: 2, height: 1 },
    { x: 2, y: 10, width: 2, height: 1 },
    { x: 6, y: 10, width: 2, height: 1 },
    { x: 10, y: 10, width: 2, height: 1 },
    { x: 14, y: 10, width: 2, height: 1 },
    { x: 2, y: 14, width: 2, height: 1 },
    { x: 6, y: 14, width: 2, height: 1 },
    { x: 10, y: 14, width: 2, height: 1 },
    { x: 14, y: 14, width: 2, height: 1 },
    // Counter
    { x: 0, y: 18, width: 20, height: 2 },
  ],

  draw: (ctx: CanvasRenderingContext2D, cellSize: number, lightsOn: boolean) => {
    // Draw shelves
    ctx.fillStyle = lightsOn ? "#8B4513" : "#4A2400"
    Store.shelves.forEach((shelf) => {
      ctx.fillRect(shelf.x * cellSize, shelf.y * cellSize, shelf.width * cellSize, shelf.height * cellSize)
    })
  },

  isCollision: (x: number, y: number): boolean => {
    return Store.shelves.some(
      (shelf) => x >= shelf.x && x < shelf.x + shelf.width && y >= shelf.y && y < shelf.y + shelf.height,
    )
  },

  rearrangeLayout: () => {
    // Randomly move some shelves
    Store.shelves.forEach((shelf, index) => {
      if (Math.random() < 0.3) {
        // 30% chance to move each shelf
        const newX = Math.floor(Math.random() * (20 - shelf.width))
        const newY = Math.floor(Math.random() * (20 - shelf.height))
        Store.shelves[index] = { ...shelf, x: newX, y: newY }
      }
    })
  },
}