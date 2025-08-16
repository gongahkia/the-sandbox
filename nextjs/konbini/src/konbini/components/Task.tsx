import type React from "react"

export interface TaskData {
  id: number
  type: "electrical" | "restock" | "trash" | "customer"
  x: number
  y: number
  completed: boolean
}

interface TaskProps {
  task: TaskData
  cellSize: number
}

export const Task: React.FC<TaskProps> = ({ task, cellSize }) => {
  const colors = {
    electrical: "#FFD700",
    restock: "#4CAF50",
    trash: "#795548",
    customer: "#2196F3",
  }

  return (
    <div
      style={{
        position: "absolute",
        left: `${task.x * cellSize}px`,
        top: `${task.y * cellSize}px`,
        width: `${cellSize}px`,
        height: `${cellSize}px`,
        backgroundColor: colors[task.type],
        border: "2px solid black",
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        fontSize: "20px",
        fontWeight: "bold",
        color: "black",
      }}
    >
      {task.type[0].toUpperCase()}
    </div>
  )
}

// Add a static draw method to the Task component
Task.draw = (ctx: CanvasRenderingContext2D, task: TaskData, cellSize: number) => {
  const colors = {
    electrical: "#FFD700",
    restock: "#4CAF50",
    trash: "#795548",
    customer: "#2196F3",
  }

  ctx.fillStyle = colors[task.type]
  ctx.fillRect(task.x * cellSize, task.y * cellSize, cellSize, cellSize)
  ctx.strokeStyle = "black"
  ctx.strokeRect(task.x * cellSize, task.y * cellSize, cellSize, cellSize)

  ctx.fillStyle = "black"
  ctx.font = "bold 20px Arial"
  ctx.textAlign = "center"
  ctx.textBaseline = "middle"
  ctx.fillText(task.type[0].toUpperCase(), task.x * cellSize + cellSize / 2, task.y * cellSize + cellSize / 2)
}