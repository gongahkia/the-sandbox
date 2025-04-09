class Game {
    constructor(canvasId) {
        this.canvas = document.getElementById(canvasId);
        this.ctx = this.canvas.getContext('2d');
        this.width = this.canvas.width;
        this.height = this.canvas.height;
        this.player = new Player(this);
        this.bullets = [];
        this.invaders = [];
        this.invaderRows = 5;
        this.invaderCols = 10;
        this.invaderDirection = 1;
        this.invaderSpeed = 0.5;
        this.score = 0;
        this.lives = 3;
        this.gameState = 'MENU';
    }

    start() {
        this.gameState = 'PLAYING';
        this.createInvaders();
        this.gameLoop();
    }

    createInvaders() {
        for (let row = 0; row < this.invaderRows; row++) {
            for (let col = 0; col < this.invaderCols; col++) {
                this.invaders.push(new Invader(this, col * 40 + 50, row * 40 + 50));
            }
        }
    }

    update() {
        if (this.gameState !== 'PLAYING') return;

        let deltaX = this.invaderDirection * this.invaderSpeed;
        let deltaY = 0;
        let changeDirection = false;

        this.invaders.forEach(invader => {
            invader.update(deltaX, deltaY);
            if (invader.x <= 0 || invader.x + invader.width >= this.width) {
                changeDirection = true;
            }
        });

        if (changeDirection) {
            this.invaderDirection *= -1;
            deltaY = 20;
            this.invaders.forEach(invader => invader.update(0, deltaY));
        }

        this.bullets = this.bullets.filter(bullet => bullet.y > 0);
        this.bullets.forEach(bullet => bullet.update());

        this.bullets.forEach(bullet => {
            this.invaders = this.invaders.filter(invader => {
                if (this.checkCollision(bullet, invader)) {
                    this.score += 10;
                    return false;
                }
                return true;
            });
        });

        if (this.invaders.some(invader => invader.y + invader.height >= this.player.y)) {
            this.lives--;
            if (this.lives <= 0) {
                this.gameState = 'GAME_OVER';
            } else {
                this.resetLevel();
            }
        }

        if (this.invaders.length === 0) {
            this.gameState = 'LEVEL_COMPLETE';
        }
    }

    resetLevel() {
        this.bullets = [];
        this.invaders = [];
        this.createInvaders();
        this.player.x = this.width / 2 - this.player.width / 2;
    }

    checkCollision(rect1, rect2) {
        return rect1.x < rect2.x + rect2.width &&
               rect1.x + rect1.width > rect2.x &&
               rect1.y < rect2.y + rect2.height &&
               rect1.y + rect1.height > rect2.y;
    }

    draw() {
        this.ctx.clearRect(0, 0, this.width, this.height);

        if (this.gameState === 'MENU') {
            this.drawMenu();
        } else if (this.gameState === 'PLAYING') {
            this.player.draw(this.ctx);
            this.invaders.forEach(invader => invader.draw(this.ctx));
            this.bullets.forEach(bullet => bullet.draw(this.ctx));
        } else if (this.gameState === 'GAME_OVER') {
            this.drawGameOver();
        } else if (this.gameState === 'LEVEL_COMPLETE') {
            this.drawLevelComplete();
        }

        this.drawHUD();
    }

    drawMenu() {
        this.ctx.fillStyle = '#ffffff';
        this.ctx.font = '30px Arial';
        this.ctx.textAlign = 'center';
        this.ctx.fillText('Space Invaders', this.width / 2, this.height / 2 - 30);
        this.ctx.font = '20px Arial';
        this.ctx.fillText('Press SPACE to start', this.width / 2, this.height / 2 + 20);
    }

    drawGameOver() {
        this.ctx.fillStyle = '#ffffff';
        this.ctx.font = '30px Arial';
        this.ctx.textAlign = 'center';
        this.ctx.fillText('GAME OVER', this.width / 2, this.height / 2 - 30);
        this.ctx.font = '20px Arial';
        this.ctx.fillText('Press SPACE to restart', this.width / 2, this.height / 2 + 20);
    }

    drawLevelComplete() {
        this.ctx.fillStyle = '#ffffff';
        this.ctx.font = '30px Arial';
        this.ctx.textAlign = 'center';
        this.ctx.fillText('LEVEL COMPLETE', this.width / 2, this.height / 2 - 30);
        this.ctx.font = '20px Arial';
        this.ctx.fillText('Press SPACE to continue', this.width / 2, this.height / 2 + 20);
    }

    drawHUD() {
        document.getElementById('score').textContent = `Score: ${this.score}`;
        document.getElementById('lives').textContent = `Lives: ${this.lives}`;
    }

    gameLoop() {
        this.update();
        this.draw();
        requestAnimationFrame(() => this.gameLoop());
    }
}

const game = new Game('gameCanvas');

document.addEventListener('keydown', (event) => {
    if (game.gameState === 'PLAYING') {
        switch (event.code) {
            case 'ArrowLeft':
                game.player.moveLeft();
                break;
            case 'ArrowRight':
                game.player.moveRight();
                break;
            case 'Space':
                game.player.shoot();
                break;
        }
    } else if (['MENU', 'GAME_OVER', 'LEVEL_COMPLETE'].includes(game.gameState)) {
        if (event.code === 'Space') {
            game.start();
        }
    }
});

document.getElementById('startButton').addEventListener('click', () => {
    if (game.gameState === 'MENU') {
        game.start();
    }
});

game.draw(); 
