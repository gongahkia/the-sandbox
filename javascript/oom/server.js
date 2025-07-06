const express = require('express');
const path = require('path');
const http = require('http');
const WebSocket = require('ws');
const os = require('os');

const app = express();
const PORT = 8000;
const WS_PORT = 8080;

app.use(express.static(path.join(__dirname, 'public')));
const httpServer = http.createServer(app);
httpServer.listen(PORT, () => {
  const interfaces = os.networkInterfaces();
  const addresses = [];
  for (const name of Object.keys(interfaces)) {
    for (const iface of interfaces[name]) {
      if (iface.family === 'IPv4' && !iface.internal) {
        addresses.push(iface.address);
      }
    }
  }
  console.log('\nFrontend available at:');
  console.log(`  http://localhost:${PORT}/index.html`);
  addresses.forEach(addr => {
    console.log(`  http://${addr}:${PORT}/index.html`);
  });
  console.log('\nStart the call in two browser windows/tabs.');
});

const wss = new WebSocket.Server({ port: WS_PORT }, () => {
  console.log(`WebSocket signaling server running on ws://localhost:${WS_PORT}`);
});

// Room structure: { [roomName]: { clients: [{ws, id, username}], callActive: false } }
const rooms = {};

function broadcastRoomList() {
  const roomList = Object.keys(rooms).map(room => ({
    name: room,
    callActive: rooms[room].callActive || false,
    userCount: rooms[room].clients.length
  }));
  const message = JSON.stringify({ type: 'roomList', rooms: roomList });
  wss.clients.forEach(client => {
    if (client.readyState === WebSocket.OPEN) {
      client.send(message);
    }
  });
}

function broadcastToRoom(room, data) {
  if (rooms[room]) {
    rooms[room].clients.forEach(clientObj => {
      if (clientObj.ws.readyState === WebSocket.OPEN) {
        clientObj.ws.send(JSON.stringify(data));
      }
    });
  }
}

function getRoomUserList(room) {
  return rooms[room] ? rooms[room].clients.map(c => ({ id: c.id, username: c.username })) : [];
}

wss.on('connection', ws => {
  ws.id = Math.random().toString(36).substr(2, 9);

  ws.send(JSON.stringify({ type: 'roomList', rooms: Object.keys(rooms).map(room => ({
    name: room,
    callActive: rooms[room].callActive || false,
    userCount: rooms[room].clients.length
  })) }));

  ws.on('message', message => {
    let data;
    try { data = JSON.parse(message); } catch (e) { return; }

    if (data.createRoom) {
      const room = data.createRoom;
      if (!rooms[room]) rooms[room] = { clients: [], callActive: false };
      broadcastRoomList();
      return;
    }

    if (data.joinRoom) {
      const room = data.joinRoom;
      if (!rooms[room]) rooms[room] = { clients: [], callActive: false };
      ws.room = room;
      ws.username = data.username || `User${Math.floor(Math.random() * 10000)}`;
      rooms[room].clients.push({ ws, id: ws.id, username: ws.username });
      ws.send(JSON.stringify({ type: 'joinedRoom', room, username: ws.username, id: ws.id, users: getRoomUserList(room) }));
      broadcastRoomList();
      broadcastToRoom(room, { type: 'userJoined', username: ws.username, id: ws.id, userCount: rooms[room].clients.length, users: getRoomUserList(room) });
      return;
    }

    if (data.leaveRoom) {
      const room = data.leaveRoom;
      if (rooms[room]) {
        rooms[room].clients = rooms[room].clients.filter(clientObj => clientObj.ws !== ws);
        if (rooms[room].clients.length === 0) delete rooms[room];
        else broadcastToRoom(room, { type: 'userLeft', username: ws.username, id: ws.id, userCount: rooms[room].clients.length, users: getRoomUserList(room) });
        broadcastRoomList();
      }
      ws.room = null;
      return;
    }

    // Call status updates
    if (typeof data.callStatus === 'boolean' && ws.room && rooms[ws.room]) {
      rooms[ws.room].callActive = data.callStatus;
      broadcastRoomList();
      broadcastToRoom(ws.room, { type: 'callStatus', callActive: data.callStatus, username: ws.username });
      return;
    }

    // Signaling: relay to specific peer or all others
    if (ws.room && rooms[ws.room]) {
      if (data.to) {
        // Direct signaling to a specific peer
        const target = rooms[ws.room].clients.find(c => c.id === data.to);
        if (target && target.ws.readyState === WebSocket.OPEN) {
          target.ws.send(JSON.stringify({ ...data, from: ws.id, username: ws.username }));
        }
      } else {
        // Broadcast to all others in the room
        rooms[ws.room].clients.forEach(clientObj => {
          if (clientObj.ws !== ws && clientObj.ws.readyState === WebSocket.OPEN) {
            clientObj.ws.send(JSON.stringify({ ...data, from: ws.id, username: ws.username }));
          }
        });
      }
    }
  });

  ws.on('close', () => {
    if (ws.room && rooms[ws.room]) {
      rooms[ws.room].clients = rooms[ws.room].clients.filter(clientObj => clientObj.ws !== ws);
      if (rooms[ws.room].clients.length === 0) delete rooms[ws.room];
      else broadcastToRoom(ws.room, { type: 'userLeft', username: ws.username, id: ws.id, userCount: rooms[ws.room].clients.length, users: getRoomUserList(ws.room) });
      broadcastRoomList();
    }
  });
});
