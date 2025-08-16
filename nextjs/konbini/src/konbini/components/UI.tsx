import type React from "react"
import type { TaskData } from "./Task"

interface UIProps {
  tasks: TaskData[]
  lightsOn: boolean
  gameOver: boolean
  score: number
}

export const UI: React.FC<UIProps> = ({ tasks, lightsOn, gameOver, score }) => {
  return (
    <div className="ml-8 w-64">
      {!gameOver && (
        <>
          <div className="mb-4 p-2 bg-gray-800 rounded">
            <span className="font-bold">Lights:</span> {lightsOn ? "On" : "Off"}
          </div>
          <div className="mb-4 p-2 bg-gray-800 rounded">
            <span className="font-bold">Score:</span> {score}
          </div>
          <div className="p-2 bg-gray-800 rounded">
            <h2 className="text-xl font-bold mb-2">Tasks:</h2>
            <ul className="list-disc pl-5">
              {tasks
                .filter((task) => !task.completed)
                .map((task) => (
                  <li key={task.id} className="mb-1">
                    {task.type}
                  </li>
                ))}
            </ul>
          </div>
        </>
      )}
      {gameOver && (
        <div className="p-4 bg-red-800 rounded">
          <h2 className="text-2xl font-bold mb-2">Game Over!</h2>
          <p>You were caught by the entity.</p>
          <p className="mt-2">Final Score: {score}</p>
        </div>
      )}
    </div>
  )
}