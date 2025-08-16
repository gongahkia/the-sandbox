class Game {
    constructor(gameWidth, gameHeight) {
        this.gameWidth = gameWidth;
        this.gameHeight = gameHeight;
        this.gameObjects = [];
        this.bricks = [];
        this.lives = 3;
        this.score = 0;
        this.gamestate = 'MENU';

        this.paddle = new Paddle(this);
        this.ball = new Ball(this);

        this.levels = [
            [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
            [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
            [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
            [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
        ];
    }

    start() {
        this.gamestate = 'RUNNING';
        this.bricks = this.buildLevel();
        this.gameObjects = [this.paddle, this.ball, ...this.bricks];
    }

    buildLevel() {
        let bricks = [];
        this.levels.forEach((row, rowIndex) => {
            row.forEach((brick, brickIndex) => {
                if (brick === 1) {
                    let position = {
                        x: 80 * brickIndex,
                        y: 75 + 24 * rowIndex,
                    };
                    bricks.push(new Brick(this, position));
                }
            });
        });
        return bricks;
    }

    update(deltaTime) {
        if (this.lives === 0) this.gamestate = 'GAMEOVER';

        if (this.gamestate === 'MENU' || this.gamestate === 'GAMEOVER') return;

        if (this.bricks.length === 0) {
            this.gamestate = 'LEVELCOMPLETE';
            this.start();
        }

        [...this.gameObjects, ...this.bricks].forEach((object) =>
            object.update(deltaTime)
        );

        this.bricks = this.bricks.filter((brick) => !brick.markedForDeletion);
    }

    draw(ctx) {
        [...this.gameObjects, ...this.bricks].forEach((object) => object.draw(ctx));

        if (this.gamestate === 'MENU') {
            ctx.rect(0, 0, this.gameWidth, this.gameHeight);
            ctx.fillStyle = 'rgba(0,0,0,0.5)';
            ctx.fill();

            ctx.font = '30px Arial';
            ctx.fillStyle = 'white';
            ctx.textAlign = 'center';
            ctx.fillText(
                'Press SPACEBAR to Start',
                this.gameWidth / 2,
                this.gameHeight / 2
            );
        }

        if (this.gamestate === 'GAMEOVER') {
            ctx.rect(0, 0, this.gameWidth, this.gameHeight);
            ctx.fillStyle = 'rgba(0,0,0,1)';
            ctx.fill();

            ctx.font = '30px Arial';
            ctx.fillStyle = 'white';
            ctx.textAlign = 'center';
            ctx.fillText('GAME OVER', this.gameWidth / 2, this.gameHeight / 2);
        }
    }

    togglePause() {
        if (this.gamestate === 'PAUSED') {
            this.gamestate = 'RUNNING';
        } else {
            this.gamestate = 'PAUSED';
        }
    }
}

let canvas = document.getElementById('gameCanvas');
let ctx = canvas.getContext('2d');

const GAME_WIDTH = 800;
const GAME_HEIGHT = 600;

let game = new Game(GAME_WIDTH, GAME_HEIGHT);

let lastTime = 0;

function gameLoop(timestamp) {
    let deltaTime = timestamp - lastTime;
    lastTime = timestamp;

    ctx.clearRect(0, 0, GAME_WIDTH, GAME_HEIGHT);

    game.update(deltaTime);
    game.draw(ctx);

    requestAnimationFrame(gameLoop);
}

document.addEventListener('keydown', (event) => {
    switch (event.key) {
        case 'ArrowLeft':
            game.paddle.moveLeft();
            break;
        case 'ArrowRight':
            game.paddle.moveRight();
            break;
        case ' ':
            if (game.gamestate === 'MENU') {
                game.start();
            }
            break;
        case 'Escape':
            game.togglePause();
            break;
    }
});

document.addEventListener('keyup', (event) => {
    switch (event.key) {
        case 'ArrowLeft':
        case 'ArrowRight':
            game.paddle.stop();
            break;
    }
});

document.getElementById('startButton').addEventListener('click', () => {
    if (game.gamestate === 'MENU') {
        game.start();
    }
});

requestAnimationFrame(gameLoop);

setInterval(() => {
    document.getElementById('score').textContent = `Score: ${game.score}`;
    document.getElementById('lives').textContent = `Lives: ${game.lives}`;
}, 100);