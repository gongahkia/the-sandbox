const express = require('express');
const app = express();
const http = require('http').createServer(app);
const io = require('socket.io')(http);

app.use(express.static('public'));

const users = {};

io.on('connection', (socket) => {
  console.log('A user connected:', socket.id);

  socket.on('set username', (username) => {
    users[socket.id] = username || 'Anonymous';
    socket.broadcast.emit('user joined', username);
  });

  socket.on('chat message', (msg) => {
    const username = users[socket.id] || 'Anonymous';
    io.emit('chat message', { username, msg });
  });

  socket.on('disconnect', () => {
    const username = users[socket.id];
    delete users[socket.id];
    if (username) {
      socket.broadcast.emit('user left', username);
    }
    console.log('A user disconnected:', socket.id);
  });
});

const PORT = process.env.PORT || 3000;
http.listen(PORT, () => {
  console.log(`Server listening on http://localhost:${PORT}`);
});