const canvas = document.getElementById('gameCanvas');
const ctx = canvas.getContext('2d');

const TILE_SIZE = 40;
const ROOM_WIDTH = 20;
const ROOM_HEIGHT = 15;

let player = { x: 1, y: 1, keys: 0 };
let enemies = [];
let collectibles = [];
let room = generateRoom();

function generateRoom() {
    let room = Array.from({ length: ROOM_HEIGHT }, () => Array(ROOM_WIDTH).fill(0));
    
    for (let i = 0; i < ROOM_HEIGHT; i++) {
        for (let j = 0; j < ROOM_WIDTH; j++) {
            room[i][j] = Math.random() < 0.2 ? 1 : 0;
        }
    }
    
    room[1][1] = 0;

    for (let i = 0; i < 3; i++) {
        let x, y;
        do {
            x = Math.floor(Math.random() * ROOM_WIDTH);
            y = Math.floor(Math.random() * ROOM_HEIGHT);
        } while (room[y][x] !== 0);
        collectibles.push({ x, y });
        room[y][x] = 'C';
    }

    for (let i = 0; i < 2; i++) {
        let x, y;
        do {
            x = Math.floor(Math.random() * ROOM_WIDTH);
            y = Math.floor(Math.random() * ROOM_HEIGHT);
        } while (room[y][x] !== 0);
        enemies.push({ x, y });
        room[y][x] = 'E';
    }

    return room;
}

function drawRoom() {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    
    for (let i = 0; i < ROOM_HEIGHT; i++) {
        for (let j = 0; j < ROOM_WIDTH; j++) {
            if (room[i][j] === 1) {
                ctx.fillStyle = 'black';
                ctx.fillRect(j * TILE_SIZE, i * TILE_SIZE, TILE_SIZE, TILE_SIZE);
            } else if (room[i][j] === 'C') {
                ctx.fillStyle = 'gold';
                ctx.fillRect(j * TILE_SIZE + TILE_SIZE / 4, i * TILE_SIZE + TILE_SIZE / 4,
                             TILE_SIZE / 2, TILE_SIZE / 2);
            } else if (room[i][j] === 'E') {
                ctx.fillStyle = 'red';
                ctx.fillRect(j * TILE_SIZE + TILE_SIZE / 4, i * TILE_SIZE + TILE_SIZE / 4,
                             TILE_SIZE / 2, TILE_SIZE / 2);
            }
        }
    }

    ctx.fillStyle = 'blue';
    ctx.fillRect(player.x * TILE_SIZE + TILE_SIZE / 4,
                 player.y * TILE_SIZE + TILE_SIZE / 4,
                 TILE_SIZE / 2, TILE_SIZE / 2);
}

document.addEventListener('keydown', function(event) {
    switch(event.key) {
        case 'ArrowUp':
            movePlayer(0, -1);
            break;
        case 'ArrowDown':
            movePlayer(0, 1);
            break;
        case 'ArrowLeft':
            movePlayer(-1, 0);
            break;
        case 'ArrowRight':
            movePlayer(1, 0);
            break;
    }
});

function movePlayer(dx, dy) {
    const newX = player.x + dx;
    const newY = player.y + dy;

    if (newX >= 0 && newX < ROOM_WIDTH && newY >= 0 && newY < ROOM_HEIGHT && room[newY][newX] !== 1) {
        player.x = newX;
        player.y = newY;

        if (room[newY][newX] === 'C') {
            player.keys++;
            collectibles = collectibles.filter(c => !(c.x === newX && c.y === newY));
            room[newY][newX] = 0; 
        }
        
        requestAnimationFrame(gameLoop); 
    }
}

function updateEnemies() {
    enemies.forEach(enemy => {
        if (Math.abs(enemy.x - player.x) <= 5 && Math.abs(enemy.y - player.y) <=5 ) { 
            if (enemy.x < player.x) enemy.x++;
            else if (enemy.x > player.x) enemy.x--;
            
            if (enemy.y < player.y) enemy.y++;
            else if (enemy.y > player.y) enemy.y--;
        }
   });
}

function gameLoop() {
   updateEnemies(); 
   drawRoom();
}

requestAnimationFrame(gameLoop);