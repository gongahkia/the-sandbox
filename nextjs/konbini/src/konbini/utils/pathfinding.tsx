import { Store } from "../components/Store"

interface Node {
  x: number
  y: number
  f: number
  g: number
  h: number
  parent: Node | null
}

export function findPath(
  start: { x: number; y: number },
  goal: { x: number; y: number },
  gridSize: number,
): { x: number; y: number }[] {
  const openSet: Node[] = []
  const closedSet: Node[] = []
  const startNode: Node = { ...start, f: 0, g: 0, h: 0, parent: null }

  openSet.push(startNode)

  while (openSet.length > 0) {
    let current = openSet[0]
    let currentIndex = 0

    for (let i = 1; i < openSet.length; i++) {
      if (openSet[i].f < current.f) {
        current = openSet[i]
        currentIndex = i
      }
    }

    openSet.splice(currentIndex, 1)
    closedSet.push(current)

    if (current.x === goal.x && current.y === goal.y) {
      const path: { x: number; y: number }[] = []
      let currentNode: Node | null = current
      while (currentNode) {
        path.push({ x: currentNode.x, y: currentNode.y })
        currentNode = currentNode.parent
      }
      return path.reverse()
    }

    const neighbors: Node[] = [
      { x: current.x + 1, y: current.y, f: 0, g: 0, h: 0, parent: current },
      { x: current.x - 1, y: current.y, f: 0, g: 0, h: 0, parent: current },
      { x: current.x, y: current.y + 1, f: 0, g: 0, h: 0, parent: current },
      { x: current.x, y: current.y - 1, f: 0, g: 0, h: 0, parent: current },
    ]

    for (const neighbor of neighbors) {
      if (
        neighbor.x < 0 ||
        neighbor.x >= gridSize ||
        neighbor.y < 0 ||
        neighbor.y >= gridSize ||
        Store.isCollision(neighbor.x, neighbor.y) ||
        closedSet.some((node) => node.x === neighbor.x && node.y === neighbor.y)
      ) {
        continue
      }

      const gScore = current.g + 1
      const hScore = Math.abs(neighbor.x - goal.x) + Math.abs(neighbor.y - goal.y)
      const fScore = gScore + hScore

      if (!openSet.some((node) => node.x === neighbor.x && node.y === neighbor.y) || gScore < neighbor.g) {
        neighbor.g = gScore
        neighbor.h = hScore
        neighbor.f = fScore

        if (!openSet.some((node) => node.x === neighbor.x && node.y === neighbor.y)) {
          openSet.push(neighbor)
        }
      }
    }
  }

  return [] // No path found
}