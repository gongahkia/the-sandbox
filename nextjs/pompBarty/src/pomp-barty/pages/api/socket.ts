import { Server as ServerIO } from "socket.io"
import type { NextApiRequest } from "next"
import type { NextApiResponseServerIO } from "../../types/next"

export const config = {
  api: {
    bodyParser: false,
  },
}

let io: ServerIO

const SocketHandler = (req: NextApiRequest, res: NextApiResponseServerIO) => {
  if (!res.socket.server.io) {
    console.log("New Socket.io server...")
    const httpServer: any = res.socket.server as any
    io = new ServerIO(httpServer, {
      path: "/api/socket",
    })
    res.socket.server.io = io
  } else {
    io = res.socket.server.io
  }

  io.on("connection", (socket) => {
    console.log("New client connected")

    socket.on("joinRoom", ({ username, roomCode }) => {
      console.log(`${username} joining room ${roomCode}`)
      socket.join(roomCode)
      if (!rooms[roomCode]) {
        rooms[roomCode] = {
          players: [],
          currentPlayerIndex: 0,
          substring: generateSubstring(),
          timeLeft: 10,
        }
      }
      rooms[roomCode].players.push({ username, score: 0 })
      socket.emit("roomJoined")
      updateGameState(roomCode)
    })

    socket.on("submitWord", ({ word, roomCode }) => {
      console.log(`Word submitted: ${word} in room ${roomCode}`)
      const room = rooms[roomCode]
      if (room && isValidWord(word, room.substring)) {
        const currentPlayer = room.players[room.currentPlayerIndex]
        currentPlayer.score += word.length
        nextTurn(roomCode)
      }
    })
  })

  res.end()
}

const rooms: { [key: string]: any } = {}

function updateGameState(roomCode: string) {
  const room = rooms[roomCode]
  if (io && room) {
    io.to(roomCode).emit("gameState", {
      players: room.players,
      currentPlayer: room.players[room.currentPlayerIndex].username,
      substring: room.substring,
      timeLeft: room.timeLeft,
    })
  }
}

function nextTurn(roomCode: string) {
  const room = rooms[roomCode]
  room.currentPlayerIndex = (room.currentPlayerIndex + 1) % room.players.length
  room.substring = generateSubstring()
  room.timeLeft = 10 
  updateGameState(roomCode)
  startTimer(roomCode)
}

function startTimer(roomCode: string) {
  const room = rooms[roomCode]
  const timer = setInterval(() => {
    room.timeLeft--
    if (io) {
      io.to(roomCode).emit("updateTime", room.timeLeft)
    }
    if (room.timeLeft <= 0) {
      clearInterval(timer)
      nextTurn(roomCode)
    }
  }, 1000)
}

function generateSubstring() {
  const substrings = ["an", "in", "th", "er", "on", "at", "en", "re", "al", "te"]
  return substrings[Math.floor(Math.random() * substrings.length)]
}

function isValidWord(word: string, substring: string) {
  return word.toLowerCase().includes(substring.toLowerCase())
}

export default SocketHandler