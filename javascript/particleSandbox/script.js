const canvas = document.getElementById("sandCanvas")
const ctx = canvas.getContext("2d")
const particleSelector = document.getElementById("particleSelector")

const CELL_SIZE = 5
let grid = []
let isDrawing = false
let selectedParticle = "sand"

const particleTypes = {
  sand: { color: "#d2b48c", update: updateSand },
  water: { color: "#4fc3f7", update: updateFluid },
  stone: { color: "#7f7f7f", update: () => {} },
  oil: { color: "#3d3d3d", update: updateFluid },
  fire: { color: "#ff4500", update: updateFire },
  steam: { color: "#e6e6e6", update: updateGas },
  wood: { color: "#8b4513", update: updateWood },
  acid: { color: "#00ff00", update: updateAcid },
  salt: { color: "#ffffff", update: updateSand },
  lava: { color: "#ff4500", update: updateLava },
  ice: { color: "#add8e6", update: updateIce },
  plant: { color: "#228b22", update: updatePlant },
  smoke: { color: "#a9a9a9", update: updateGas },
  gunpowder: { color: "#36454f", update: updateGunpowder },
  electricity: { color: "#ffff00", update: updateElectricity },
}

function initializeCanvas() {
  canvas.width = window.innerWidth
  canvas.height = window.innerHeight
  const cols = Math.floor(canvas.width / CELL_SIZE)
  const rows = Math.floor(canvas.height / CELL_SIZE)
  grid = Array(rows)
    .fill()
    .map(() => Array(cols).fill(null))
}

function createParticleSelector() {
  Object.entries(particleTypes).forEach(([type, properties]) => {
    const button = document.createElement("button")
    button.className = "particle-option"
    button.style.backgroundColor = properties.color
    button.title = type.charAt(0).toUpperCase() + type.slice(1)
    button.addEventListener("click", () => selectParticle(type))
    particleSelector.appendChild(button)
  })
  selectParticle("sand")
}

function selectParticle(type) {
  selectedParticle = type
  document.querySelectorAll(".particle-option").forEach((btn) => {
    btn.classList.toggle("selected", btn.title.toLowerCase() === type)
  })
}

function updateParticles() {
  const newGrid = grid.map((row) => [...row])

  for (let y = grid.length - 1; y >= 0; y--) {
    for (let x = 0; x < grid[y].length; x++) {
      const particle = grid[y][x]
      if (particle) {
        particleTypes[particle].update(x, y, grid, newGrid)
      }
    }
  }

  grid = newGrid
}

function updateSand(x, y, grid, newGrid) {
  if (y + 1 < grid.length && !grid[y + 1][x]) {
    newGrid[y][x] = null
    newGrid[y + 1][x] = "sand"
  } else if (y + 1 < grid.length) {
    const random = Math.random()
    if (random < 0.5 && x - 1 >= 0 && !grid[y + 1][x - 1]) {
      newGrid[y][x] = null
      newGrid[y + 1][x - 1] = "sand"
    } else if (random >= 0.5 && x + 1 < grid[y].length && !grid[y + 1][x + 1]) {
      newGrid[y][x] = null
      newGrid[y + 1][x + 1] = "sand"
    }
  }
}

function updateFluid(x, y, grid, newGrid) {
  if (y + 1 < grid.length && !grid[y + 1][x]) {
    newGrid[y][x] = null
    newGrid[y + 1][x] = grid[y][x]
  } else {
    const random = Math.random()
    if (random < 0.3 && x - 1 >= 0 && !grid[y][x - 1]) {
      newGrid[y][x] = null
      newGrid[y][x - 1] = grid[y][x]
    } else if (random >= 0.7 && x + 1 < grid[y].length && !grid[y][x + 1]) {
      newGrid[y][x] = null
      newGrid[y][x + 1] = grid[y][x]
    }
  }
}

function updateFire(x, y, grid, newGrid) {
  if (Math.random() < 0.1) {
    newGrid[y][x] = "smoke"
  } else {
    if (y > 0 && !grid[y - 1][x]) {
      newGrid[y - 1][x] = "fire"
    }
    getNeighbors(x, y, grid).forEach(([nx, ny]) => {
      switch (grid[ny][nx]) {
        case "water":
          newGrid[y][x] = "steam"
          break
        case "wood":
        case "plant":
          newGrid[ny][nx] = "fire"
          break
        case "ice":
          newGrid[ny][nx] = "water"
          break
      }
    })
  }
}

function updateGas(x, y, grid, newGrid) {
  if (y > 0 && !grid[y - 1][x]) {
    newGrid[y][x] = null
    newGrid[y - 1][x] = grid[y][x]
  } else {
    const random = Math.random()
    if (random < 0.3 && x - 1 >= 0 && !grid[y][x - 1]) {
      newGrid[y][x] = null
      newGrid[y][x - 1] = grid[y][x]
    } else if (random >= 0.7 && x + 1 < grid[y].length && !grid[y][x + 1]) {
      newGrid[y][x] = null
      newGrid[y][x + 1] = grid[y][x]
    }
  }
  if (Math.random() < 0.01) {
    newGrid[y][x] = null
  }
}

function updateWood(x, y, grid, newGrid) {
  if (hasNeighbor(x, y, grid, "fire")) {
    newGrid[y][x] = "fire"
  }
}

function updateAcid(x, y, grid, newGrid) {
  updateFluid(x, y, grid, newGrid)
  getNeighbors(x, y, grid).forEach(([nx, ny]) => {
    if (grid[ny][nx] && grid[ny][nx] !== "acid" && grid[ny][nx] !== "water") {
      if (Math.random() < 0.1) {
        newGrid[ny][nx] = null
      }
    }
  })
}

function updateLava(x, y, grid, newGrid) {
  updateFluid(x, y, grid, newGrid)
  getNeighbors(x, y, grid).forEach(([nx, ny]) => {
    switch (grid[ny][nx]) {
      case "water":
        newGrid[ny][nx] = "stone"
        break
      case "ice":
        newGrid[ny][nx] = "water"
        break
    }
  })
}

function updateIce(x, y, grid, newGrid) {
  if (hasNeighbor(x, y, grid, "water")) {
    if (Math.random() < 0.01) {
      const [nx, ny] = getRandomNeighbor(x, y, grid, "water")
      newGrid[ny][nx] = "ice"
    }
  }
  if (hasNeighbor(x, y, grid, "fire") || hasNeighbor(x, y, grid, "lava")) {
    newGrid[y][x] = "water"
  }
}

function updatePlant(x, y, grid, newGrid) {
  if (hasNeighbor(x, y, grid, "water")) {
    if (Math.random() < 0.01) {
      const [nx, ny] = getRandomEmptyNeighbor(x, y, grid)
      if (nx !== -1 && ny !== -1) {
        newGrid[ny][nx] = "plant"
      }
    }
  }
}

function updateGunpowder(x, y, grid, newGrid) {
  updateSand(x, y, grid, newGrid)
  if (hasNeighbor(x, y, grid, "fire")) {
    newGrid[y][x] = "fire"
    getNeighbors(x, y, grid).forEach(([nx, ny]) => {
      if (grid[ny][nx] === "gunpowder") {
        newGrid[ny][nx] = "fire"
      }
    })
  }
}

function updateElectricity(x, y, grid, newGrid) {
  const [nx, ny] = getRandomNeighbor(x, y, grid)
  if (nx !== -1 && ny !== -1) {
    newGrid[y][x] = null
    newGrid[ny][nx] = "electricity"
  } else {
    newGrid[y][x] = null
  }
  getNeighbors(x, y, grid).forEach(([nx, ny]) => {
    switch (grid[ny][nx]) {
      case "water":
        newGrid[ny][nx] = "steam"
        break
      case "plant":
        newGrid[ny][nx] = "fire"
        break
    }
  })
}

function getNeighbors(x, y, grid) {
  const neighbors = []
  for (let dy = -1; dy <= 1; dy++) {
    for (let dx = -1; dx <= 1; dx++) {
      if (dx === 0 && dy === 0) continue
      const nx = x + dx
      const ny = y + dy
      if (nx >= 0 && nx < grid[0].length && ny >= 0 && ny < grid.length) {
        neighbors.push([nx, ny])
      }
    }
  }
  return neighbors
}

function hasNeighbor(x, y, grid, type) {
  return getNeighbors(x, y, grid).some(([nx, ny]) => grid[ny][nx] === type)
}

function getRandomNeighbor(x, y, grid, type) {
  const neighbors = getNeighbors(x, y, grid)
  const validNeighbors = type
    ? neighbors.filter(([nx, ny]) => grid[ny][nx] === type)
    : neighbors.filter(([nx, ny]) => !grid[ny][nx])
  if (validNeighbors.length === 0) return [-1, -1]
  return validNeighbors[Math.floor(Math.random() * validNeighbors.length)]
}

function getRandomEmptyNeighbor(x, y, grid) {
  return getRandomNeighbor(x, y, grid)
}

function drawParticles() {
  ctx.clearRect(0, 0, canvas.width, canvas.height)

  grid.forEach((row, y) => {
    row.forEach((particle, x) => {
      if (particle) {
        ctx.fillStyle = particleTypes[particle].color
        ctx.fillRect(x * CELL_SIZE, y * CELL_SIZE, CELL_SIZE, CELL_SIZE)
      }
    })
  })
}

function animate() {
  updateParticles()
  drawParticles()
  requestAnimationFrame(animate)
}

canvas.addEventListener("pointerdown", (e) => {
  isDrawing = true
  handlePointerEvent(e)
})

canvas.addEventListener("pointermove", handlePointerEvent)

canvas.addEventListener("pointerup", () => {
  isDrawing = false
})

canvas.addEventListener("pointerleave", () => {
  isDrawing = false
})

function handlePointerEvent(event) {
  if (!isDrawing) return

  const rect = canvas.getBoundingClientRect()
  const x = Math.floor((event.clientX - rect.left) / CELL_SIZE)
  const y = Math.floor((event.clientY - rect.top) / CELL_SIZE)

  if (x >= 0 && x < grid[0].length && y >= 0 && y < grid.length) {
    grid[y][x] = selectedParticle
  }
}

window.addEventListener("resize", () => {
  initializeCanvas()
})

initializeCanvas()
createParticleSelector()
animate()