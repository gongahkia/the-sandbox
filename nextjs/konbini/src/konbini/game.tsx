"use client"

import { useEffect, useRef, useState } from "react"
import { Player } from "./components/Player"
import { Entity, createEntity } from "./components/Entity"
import { Store } from "./components/Store"
import { UI } from "./components/UI"
import { Task, type TaskData } from "./components/Task"
import { PlayerEffects, type PlayerEffects as PlayerEffectsType } from "./components/PlayerEffects"
import { findPath } from "./utils/pathfinding"

const GRID_SIZE = 20
const CELL_SIZE = 20
const BASE_ENTITY_MOVE_INTERVAL = 500
const LIGHTS_OFF_SPEED_MULTIPLIER = 2
const ENTITY_FOV_RANGE = 5

interface Position {
  x: number
  y: number
}

const addTask = (
  type: "electrical" | "restock" | "trash" | "customer",
  setTasks: React.Dispatch<React.SetStateAction<TaskData[]>>,
) => {
  let newX, newY
  do {
    newX = Math.floor(Math.random() * GRID_SIZE)
    newY = Math.floor(Math.random() * GRID_SIZE)
  } while (!isValidTaskPosition(newX, newY))

  const newTask: TaskData = {
    id: Date.now(),
    type,
    x: newX,
    y: newY,
    completed: false,
  }
  setTasks((prevTasks) => [...prevTasks, newTask])
}

export default function Game() {
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const [playerPosition, setPlayerPosition] = useState<Position>({ x: 5, y: 5 })
  const [entities, setEntities] = useState<ReturnType<typeof createEntity>[]>([])
  const [lightsOn, setLightsOn] = useState(true)
  const [tasks, setTasks] = useState<TaskData[]>([])
  const [gameOver, setGameOver] = useState(false)
  const [activeTask, setActiveTask] = useState<TaskData | null>(null)
  const [taskPopup, setTaskPopup] = useState<{ task: TaskData; key: string } | null>(null)
  const [entityMoveInterval, setEntityMoveInterval] = useState(BASE_ENTITY_MOVE_INTERVAL)
  const [score, setScore] = useState(0)
  const [playerEffects, setPlayerEffects] = useState<PlayerEffectsType>({
    hasMigraine: false,
    isHallucinating: false,
  })

  useEffect(() => {
    // Initialize entities
    const initialEntities = [
      createEntity("yokai", { x: 15, y: 15 }),
      createEntity("obake", { x: 3, y: 17 }),
      createEntity("kappa", { x: 17, y: 3 }),
      createEntity("tengu", { x: 1, y: 1 }),
    ]
    setEntities(initialEntities)
  }, [])

  useEffect(() => {
    const canvas = canvasRef.current
    const ctx = canvas?.getContext("2d")
    if (!ctx) return

    const drawGame = () => {
      ctx.clearRect(0, 0, GRID_SIZE * CELL_SIZE, GRID_SIZE * CELL_SIZE)

      // Draw grid and store layout
      Store.draw(ctx, CELL_SIZE, lightsOn)

      // Draw entities
      entities.forEach((entity) => {
        Entity.draw(ctx, entity, CELL_SIZE, lightsOn)
      })

      // Draw player
      Player.draw(ctx, playerPosition, CELL_SIZE)

      // Draw tasks
      tasks.forEach((task) => {
        if (!task.completed) {
          Task.draw(ctx, task, CELL_SIZE)
        }
      })

      // Draw player effects
      PlayerEffects.draw(ctx, playerEffects, playerPosition, CELL_SIZE)

      if (gameOver) {
        ctx.fillStyle = "rgba(0, 0, 0, 0.7)"
        ctx.fillRect(0, 0, GRID_SIZE * CELL_SIZE, GRID_SIZE * CELL_SIZE)
        ctx.font = "30px Arial"
        ctx.fillStyle = "red"
        ctx.textAlign = "center"
        ctx.fillText("GAME OVER", (GRID_SIZE * CELL_SIZE) / 2, (GRID_SIZE * CELL_SIZE) / 2)
      }
    }

    drawGame()

    const animationFrame = requestAnimationFrame(function animate() {
      drawGame()
      requestAnimationFrame(animate)
    })

    return () => cancelAnimationFrame(animationFrame)
  }, [playerPosition, entities, lightsOn, gameOver, tasks, playerEffects])

  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      if (gameOver) return

      if (taskPopup) {
        if (e.key.toLowerCase() === taskPopup.key.toLowerCase()) {
          completeTask(taskPopup.task)
        }
        return
      }

      const newPosition: Position = { ...playerPosition }

      switch (e.key) {
        case "ArrowUp":
          newPosition.y = Math.max(0, playerPosition.y - 1)
          break
        case "ArrowDown":
          newPosition.y = Math.min(GRID_SIZE - 1, playerPosition.y + 1)
          break
        case "ArrowLeft":
          newPosition.x = Math.max(0, playerPosition.x - 1)
          break
        case "ArrowRight":
          newPosition.x = Math.min(GRID_SIZE - 1, playerPosition.x + 1)
          break
      }

      if (!Store.isCollision(newPosition.x, newPosition.y)) {
        setPlayerPosition(newPosition)

        // Check for collision with entities
        entities.forEach((entity) => {
          if (entity.position.x === newPosition.x && entity.position.y === newPosition.y) {
            setGameOver(true)
          }
        })

        // Check for task at new position
        const taskAtNewPosition = tasks.find((task) => task.x === newPosition.x && task.y === newPosition.y)
        if (taskAtNewPosition && !taskAtNewPosition.completed) {
          setActiveTask(taskAtNewPosition)
          const key = String.fromCharCode(65 + Math.floor(Math.random() * 26))
          setTaskPopup({ task: taskAtNewPosition, key })
        } else {
          setActiveTask(null)
          setTaskPopup(null)
        }
      }
    }

    window.addEventListener("keydown", handleKeyDown)
    return () => window.removeEventListener("keydown", handleKeyDown)
  }, [playerPosition, gameOver, tasks, taskPopup, entities])

  const completeTask = (task: TaskData) => {
    setTasks((prevTasks) => prevTasks.map((t) => (t.id === task.id ? { ...t, completed: true } : t)))
    setActiveTask(null)
    setTaskPopup(null)
    setScore((prevScore) => prevScore + 20)

    if (task.type === "electrical") {
      setLightsOn(true)
    }
  }

  useEffect(() => {
    if (gameOver) return

    const moveEntities = () => {
      setEntities((prevEntities) =>
        prevEntities.map((entity) => {
          const path = findPath(entity.position, playerPosition, GRID_SIZE)
          if (path.length > 1) {
            const nextPosition = path[1]
            if (Entity.checkCollision(nextPosition, playerPosition)) {
              setGameOver(true)
            }
            return { ...entity, position: nextPosition }
          }
          return entity
        }),
      )

      // Randomly cause catastrophes
      if (Math.random() < 0.05) {
        // 5% chance each interval
        causeCatastrophe()
      }

      // Randomly disguise entities
      setEntities((prevEntities) =>
        prevEntities.map((entity) => ({
          ...entity,
          isDisguised: Math.random() < 0.1, // 10% chance to be disguised
        })),
      )
    }

    const entityInterval = setInterval(moveEntities, entityMoveInterval)
    return () => clearInterval(entityInterval)
  }, [playerPosition, gameOver, entityMoveInterval])

  const causeCatastrophe = () => {
    const catastropheType = Math.random()
    if (catastropheType < 0.3) {
      // Cause migraine
      setPlayerEffects((prev) => ({ ...prev, hasMigraine: true }))
      setTimeout(() => setPlayerEffects((prev) => ({ ...prev, hasMigraine: false })), 5000)
    } else if (catastropheType < 0.6) {
      // Cause hallucinations
      setPlayerEffects((prev) => ({ ...prev, isHallucinating: true }))
      setTimeout(() => setPlayerEffects((prev) => ({ ...prev, isHallucinating: false })), 5000)
    } else {
      // Rearrange store layout
      Store.rearrangeLayout()
    }
  }

  useEffect(() => {
    if (gameOver) return

    const flickerLights = () => {
      if (Math.random() < 0.1) {
        setLightsOn(false)
        setEntityMoveInterval(BASE_ENTITY_MOVE_INTERVAL / LIGHTS_OFF_SPEED_MULTIPLIER)
        // Add an electrical task when lights go off
        addTask("electrical", setTasks)
      }
    }

    const lightsInterval = setInterval(flickerLights, 5000)
    return () => clearInterval(lightsInterval)
  }, [gameOver]) // Removed unnecessary setTasks dependency

  useEffect(() => {
    setEntityMoveInterval(
      lightsOn ? BASE_ENTITY_MOVE_INTERVAL : BASE_ENTITY_MOVE_INTERVAL / LIGHTS_OFF_SPEED_MULTIPLIER,
    )
  }, [lightsOn])

  useEffect(() => {
    if (gameOver) return

    const generateTask = () => {
      const taskTypes: ("restock" | "trash" | "customer")[] = ["restock", "trash", "customer"]
      addTask(taskTypes[Math.floor(Math.random() * taskTypes.length)], setTasks)
    }

    const taskInterval = setInterval(generateTask, 10000)
    return () => clearInterval(taskInterval)
  }, [gameOver]) // Removed unnecessary setTasks dependency

  useEffect(() => {
    if (gameOver) return

    const timer = setInterval(() => {
      setScore((prevScore) => prevScore + 1)
    }, 1000)

    return () => clearInterval(timer)
  }, [gameOver])

  const isValidTaskPosition = (x: number, y: number): boolean => {
    return !Store.isCollision(x, y) && !tasks.some((task) => task.x === x && task.y === y)
  }

  const restartGame = () => {
    let newPlayerPosition: Position
    do {
      newPlayerPosition = {
        x: Math.floor(Math.random() * GRID_SIZE),
        y: Math.floor(Math.random() * GRID_SIZE),
      }
    } while (Store.isCollision(newPlayerPosition.x, newPlayerPosition.y))

    const newEntities = [
      createEntity("yokai", { x: 15, y: 15 }),
      createEntity("obake", { x: 3, y: 17 }),
      createEntity("kappa", { x: 17, y: 3 }),
      createEntity("tengu", { x: 1, y: 1 }),
    ]

    setPlayerPosition(newPlayerPosition)
    setEntities(newEntities)
    setLightsOn(true)
    setTasks([])
    setGameOver(false)
    setActiveTask(null)
    setTaskPopup(null)
    setEntityMoveInterval(BASE_ENTITY_MOVE_INTERVAL)
    setScore(0)
    setPlayerEffects({ hasMigraine: false, isHallucinating: false })
  }

  return (
    <div className="flex items-center justify-center min-h-screen bg-gray-900 text-white">
      <div className="relative">
        <h1 className="text-2xl font-bold mb-4">Night Shift Horror</h1>
        <canvas
          ref={canvasRef}
          width={GRID_SIZE * CELL_SIZE}
          height={GRID_SIZE * CELL_SIZE}
          className="border border-gray-700"
        />
        {taskPopup && (
          <div className="absolute top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 bg-white text-black p-4 rounded shadow-lg">
            <p>
              Press '{taskPopup.key}' to complete the {taskPopup.task.type} task!
            </p>
          </div>
        )}
        {gameOver && (
          <button
            onClick={restartGame}
            className="mt-4 px-4 py-2 bg-red-600 text-white rounded hover:bg-red-700 transition-colors"
          >
            Restart Game
          </button>
        )}
      </div>
      <UI tasks={tasks} lightsOn={lightsOn} gameOver={gameOver} score={score} />
    </div>
  )
}