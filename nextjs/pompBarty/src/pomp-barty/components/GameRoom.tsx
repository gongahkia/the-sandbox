import { useState, useEffect } from "react"
import type { Socket } from "socket.io-client"

interface Player {
  username: string
  score: number
}

interface GameRoomProps {
  socket: Socket
  username: string
  roomCode: string
}

export default function GameRoom({ socket, username, roomCode }: GameRoomProps) {
  const [players, setPlayers] = useState<Player[]>([])
  const [currentPlayer, setCurrentPlayer] = useState("")
  const [substring, setSubstring] = useState("")
  const [timeLeft, setTimeLeft] = useState(0)
  const [word, setWord] = useState("")

  useEffect(() => {
    console.log("GameRoom component mounted")

    socket.on("gameState", (gameState) => {
      console.log("Received game state:", gameState)
      setPlayers(gameState.players)
      setCurrentPlayer(gameState.currentPlayer)
      setSubstring(gameState.substring)
      setTimeLeft(gameState.timeLeft)
    })

    socket.on("updateTime", (time) => {
      console.log("Time updated:", time)
      setTimeLeft(time)
    })

    return () => {
      console.log("GameRoom component unmounting")
      socket.off("gameState")
      socket.off("updateTime")
    }
  }, [socket])

  const submitWord = () => {
    console.log(`Submitting word: ${word}`)
    socket.emit("submitWord", { word, roomCode })
    setWord("")
  }

  return (
    <div className="flex flex-col items-center justify-center min-h-screen bg-gray-100">
      <div className="p-6 bg-white rounded shadow-md w-full max-w-md">
        <h2 className="text-xl font-bold mb-4">Room: {roomCode}</h2>
        <div className="mb-4">
          <h3 className="font-bold">Players:</h3>
          <ul>
            {players.map((player) => (
              <li key={player.username}>
                {player.username}: {player.score}
              </li>
            ))}
          </ul>
        </div>
        <div className="mb-4">
          <p>Current Player: {currentPlayer}</p>
          <p>Substring: {substring}</p>
          <p>Time Left: {timeLeft}s</p>
        </div>
        <input
          type="text"
          value={word}
          onChange={(e) => setWord(e.target.value)}
          className="w-full p-2 mb-2 border rounded"
          placeholder="Enter a word"
        />
        <button onClick={submitWord} className="w-full p-2 bg-blue-500 text-white rounded hover:bg-blue-600">
          Submit
        </button>
      </div>
    </div>
  )
}