const ws = new WebSocket('ws://localhost:8080');

const dashboard = document.getElementById('dashboard');
const roomListEl = document.getElementById('roomList');
const createRoomBtn = document.getElementById('createRoomBtn');
const newRoomInput = document.getElementById('newRoomInput');
const usernameInput = document.getElementById('usernameInput');

const roomSection = document.getElementById('roomSection');
const roomTitle = document.getElementById('roomTitle');
const roomEvents = document.getElementById('roomEvents');
const startBtn = document.getElementById('startBtn');
const hangupBtn = document.getElementById('hangupBtn');
const leaveBtn = document.getElementById('leaveBtn');
const localVideo = document.getElementById('localVideo');
const localUsernameLabel = document.getElementById('localUsername');
const remoteVideos = document.getElementById('remoteVideos');

let currentRoom = null;
let myId = null;
let myUsername = '';
let localStream = null;
const peerConnections = {}; // { [userId]: RTCPeerConnection }
const remoteStreams = {};   // { [userId]: MediaStream }
const remoteUsernames = {}; // { [userId]: username }

const config = { iceServers: [{ urls: 'stun:stun.l.google.com:19302' }] };

// --- WebSocket message handling ---
ws.onmessage = async (msg) => {
  const data = JSON.parse(msg.data);

  if (data.type === 'roomList') {
    updateRoomList(data.rooms);
  }

  if (data.type === 'joinedRoom') {
    myUsername = data.username;
    myId = data.id;
    enterRoom(data.room, data.users);
  }

  if (data.type === 'userJoined') {
    addRoomEvent(`${data.username} joined the OOM room. (${data.userCount} user${data.userCount === 1 ? '' : 's'})`);
    updateRemoteUserList(data.users);
    if (currentRoom && data.id !== myId) {
      // Initiate connection to the new user
      await createPeerConnection(data.id, data.username, true);
    }
  }

  if (data.type === 'userLeft') {
    addRoomEvent(`${data.username} left the room. (${data.userCount} user${data.userCount === 1 ? '' : 's'})`);
    removeRemoteVideo(data.id);
    closePeerConnection(data.id);
    updateRemoteUserList(data.users);
  }

  if (data.type === 'callStatus') {
    if (data.callActive) {
      addRoomEvent(`${data.username} started a call.`);
    } else {
      addRoomEvent(`Call ended.`);
    }
  }

  // --- Signaling for mesh connections ---
  if (data.offer && data.from && data.username) {
    await createPeerConnection(data.from, data.username, false);
    await peerConnections[data.from].setRemoteDescription(new RTCSessionDescription(data.offer));
    const answer = await peerConnections[data.from].createAnswer();
    await peerConnections[data.from].setLocalDescription(answer);
    ws.send(JSON.stringify({ room: currentRoom, answer, to: data.from }));
  } else if (data.answer && data.from) {
    if (peerConnections[data.from]) {
      await peerConnections[data.from].setRemoteDescription(new RTCSessionDescription(data.answer));
    }
  } else if (data.candidate && data.from) {
    if (peerConnections[data.from]) {
      await peerConnections[data.from].addIceCandidate(new RTCIceCandidate(data.candidate));
    }
  }
};

// --- Room list UI ---
function updateRoomList(rooms) {
  roomListEl.innerHTML = '';
  if (rooms.length === 0) {
    const li = document.createElement('li');
    li.textContent = 'No rooms available';
    roomListEl.appendChild(li);
    return;
  }
  rooms.forEach(room => {
    const li = document.createElement('li');
    li.textContent = `${room.name} (${room.userCount} user${room.userCount === 1 ? '' : 's'}) `;
    const statusSpan = document.createElement('span');
    statusSpan.textContent = room.callActive ? ' (In Call)' : ' (Idle)';
    statusSpan.className = room.callActive ? 'status-active' : 'status-idle';
    li.appendChild(statusSpan);
    const joinBtn = document.createElement('button');
    joinBtn.textContent = 'Join';
    joinBtn.onclick = () => joinRoom(room.name);
    li.appendChild(joinBtn);
    roomListEl.appendChild(li);
  });
}

// --- Create Room ---
createRoomBtn.onclick = () => {
  const room = newRoomInput.value.trim();
  if (!room) {
    alert('Please enter a room name.');
    return;
  }
  ws.send(JSON.stringify({ createRoom: room }));
  joinRoom(room);
  newRoomInput.value = '';
};

// --- Join Room ---
function joinRoom(room) {
  const username = usernameInput.value.trim();
  ws.send(JSON.stringify({ joinRoom: room, username }));
}

// --- Enter Room UI ---
function enterRoom(room, users) {
  currentRoom = room;
  dashboard.style.display = 'none';
  roomSection.style.display = '';
  roomTitle.textContent = 'Room: ' + room;
  startBtn.disabled = false;
  hangupBtn.disabled = true;
  roomEvents.innerHTML = '';
  localUsernameLabel.textContent = myUsername;
  updateRemoteUserList(users);
  // Initiate connections to all existing users
  users.forEach(async user => {
    if (user.id !== myId) {
      await createPeerConnection(user.id, user.username, true);
    }
  });
}

// --- Leave Room ---
leaveBtn.onclick = () => {
  cleanup();
  ws.send(JSON.stringify({ leaveRoom: currentRoom }));
  currentRoom = null;
  dashboard.style.display = '';
  roomSection.style.display = 'none';
};

// --- Start Call ---
startBtn.onclick = async () => {
  await ensureLocalStream();
  for (const userId in peerConnections) {
    if (peerConnections[userId].signalingState === 'stable') {
      const offer = await peerConnections[userId].createOffer();
      await peerConnections[userId].setLocalDescription(offer);
      ws.send(JSON.stringify({ room: currentRoom, offer, to: userId }));
    }
  }
  ws.send(JSON.stringify({ room: currentRoom, callStatus: true }));
  startBtn.disabled = true;
  hangupBtn.disabled = false;
};

// --- Hang Up ---
hangupBtn.onclick = () => {
  ws.send(JSON.stringify({ room: currentRoom, callStatus: false }));
  cleanupPeerConnections();
  hangupBtn.disabled = true;
  startBtn.disabled = false;
};

// --- Peer Connection Management ---
async function createPeerConnection(userId, username, isInitiator) {
  if (peerConnections[userId]) return;
  remoteUsernames[userId] = username;
  const pc = new RTCPeerConnection(config);
  peerConnections[userId] = pc;

  await ensureLocalStream();
  localStream.getTracks().forEach(track => pc.addTrack(track, localStream));

  pc.onicecandidate = (event) => {
    if (event.candidate) {
      ws.send(JSON.stringify({ room: currentRoom, candidate: event.candidate, to: userId }));
    }
  };

  pc.ontrack = (event) => {
    if (!remoteStreams[userId]) {
      remoteStreams[userId] = new MediaStream();
      addRemoteVideo(userId, username, remoteStreams[userId]);
    }
    event.streams[0].getTracks().forEach(track => {
      if (!remoteStreams[userId].getTracks().find(t => t.id === track.id)) {
        remoteStreams[userId].addTrack(track);
      }
    });
    updateRemoteVideo(userId, remoteStreams[userId]);
  };

  pc.onconnectionstatechange = () => {
    if (pc.connectionState === 'disconnected' || pc.connectionState === 'closed') {
      removeRemoteVideo(userId);
      closePeerConnection(userId);
    }
  };

  // If initiator, create offer after local stream is added
  if (isInitiator && localStream) {
    // Offer will be created in startBtn.onclick
  }
}

function closePeerConnection(userId) {
  if (peerConnections[userId]) {
    peerConnections[userId].close();
    delete peerConnections[userId];
  }
  if (remoteStreams[userId]) {
    delete remoteStreams[userId];
  }
  removeRemoteVideo(userId);
}

function cleanupPeerConnections() {
  Object.keys(peerConnections).forEach(userId => closePeerConnection(userId));
  remoteVideos.innerHTML = '';
}

// --- Local Stream ---
async function ensureLocalStream() {
  if (localStream) return;
  localStream = await navigator.mediaDevices.getUserMedia({ video: true, audio: true });
  localVideo.srcObject = localStream;
}

// --- Cleanup ---
function cleanup() {
  cleanupPeerConnections();
  if (localStream) {
    localStream.getTracks().forEach(track => track.stop());
    localStream = null;
  }
  localVideo.srcObject = null;
  startBtn.disabled = false;
  hangupBtn.disabled = true;
  roomEvents.innerHTML = '';
  remoteVideos.innerHTML = '';
}

// --- Room Events UI ---
function addRoomEvent(msg) {
  const div = document.createElement('div');
  div.textContent = msg;
  roomEvents.appendChild(div);
}

// --- Remote Video UI ---
function addRemoteVideo(userId, username, stream) {
  if (document.getElementById('video-' + userId)) return;
  const container = document.createElement('div');
  container.id = 'container-' + userId;
  container.style.display = 'inline-block';
  const video = document.createElement('video');
  video.id = 'video-' + userId;
  video.className = 'remote-video';
  video.autoplay = true;
  video.playsInline = true;
  video.srcObject = stream;
  const label = document.createElement('span');
  label.className = 'username-label';
  label.textContent = username;
  container.appendChild(video);
  container.appendChild(label);
  remoteVideos.appendChild(container);
}

function updateRemoteVideo(userId, stream) {
  const video = document.getElementById('video-' + userId);
  if (video) video.srcObject = stream;
}

function removeRemoteVideo(userId) {
  const container = document.getElementById('container-' + userId);
  if (container) container.remove();
  if (remoteStreams[userId]) delete remoteStreams[userId];
  if (peerConnections[userId]) closePeerConnection(userId);
}

// --- Update remote user list (for new joins/leaves) ---
function updateRemoteUserList(users) {
  // Remove videos for users no longer present
  const userIds = users.map(u => u.id);
  Object.keys(remoteStreams).forEach(userId => {
    if (!userIds.includes(userId)) removeRemoteVideo(userId);
  });
  // Add videos for new users (if any)
  users.forEach(user => {
    if (user.id !== myId && !peerConnections[user.id]) {
      // Not connected yet, will connect on join event
    }
  });
}
