"use client"

import { useState, useEffect } from "react"
import { io, type Socket } from "socket.io-client"
import GameRoom from "../components/GameRoom"

let socket: Socket

export default function Home() {
  const [connected, setConnected] = useState(false)
  const [username, setUsername] = useState("")
  const [roomCode, setRoomCode] = useState("")
  const [inRoom, setInRoom] = useState(false)

  useEffect(() => {
    socketInitializer()
  }, [])

  const socketInitializer = async () => {
    await fetch("/api/socket")
    socket = io({
      path: "/api/socket",
    })

    socket.on("connect", () => {
      console.log("Connected to Socket.IO server")
      setConnected(true)
    })

    socket.on("roomJoined", () => {
      console.log("Joined room successfully")
      setInRoom(true)
    })

    socket.on("connect_error", (err) => {
      console.log(`Connection error: ${err.message}`)
    })
  }

  const joinRoom = () => {
    if (username && roomCode) {
      console.log(`Attempting to join room ${roomCode} as ${username}`)
      socket.emit("joinRoom", { username, roomCode })
    }
  }

  if (!connected) return <div>Connecting to server...</div>

  if (!inRoom) {
    return (
      <div className="flex flex-col items-center justify-center min-h-screen bg-gray-100">
        <div className="p-6 bg-white rounded shadow-md">
          <h1 className="text-2xl font-bold mb-4">Pomp Barty</h1>
          <input
            type="text"
            placeholder="Username"
            value={username}
            onChange={(e) => setUsername(e.target.value)}
            className="w-full p-2 mb-2 border rounded"
          />
          <input
            type="text"
            placeholder="Room Code"
            value={roomCode}
            onChange={(e) => setRoomCode(e.target.value)}
            className="w-full p-2 mb-4 border rounded"
          />
          <button onClick={joinRoom} className="w-full p-2 bg-blue-500 text-white rounded hover:bg-blue-600">
            Join Room
          </button>
        </div>
      </div>
    )
  }

  return <GameRoom socket={socket} username={username} roomCode={roomCode} />
}