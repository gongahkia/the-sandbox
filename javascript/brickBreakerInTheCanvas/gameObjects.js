class Paddle {
    constructor(game) {
        this.gameWidth = game.gameWidth;
        this.width = 150;
        this.height = 20;
        this.maxSpeed = 7;
        this.speed = 0;
        this.position = {
            x: game.gameWidth / 2 - this.width / 2,
            y: game.gameHeight - this.height - 10,
        };
    }

    moveLeft() {
        this.speed = -this.maxSpeed;
    }

    moveRight() {
        this.speed = this.maxSpeed;
    }

    stop() {
        this.speed = 0;
    }

    draw(ctx) {
        ctx.fillStyle = '#0095DD';
        ctx.fillRect(this.position.x, this.position.y, this.width, this.height);
    }

    update(deltaTime) {
        this.position.x += this.speed;
        if (this.position.x < 0) this.position.x = 0;
        if (this.position.x + this.width > this.gameWidth)
            this.position.x = this.gameWidth - this.width;
    }
}

class Ball {
    constructor(game) {
        this.image = document.createElement('img');
        this.image.src = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAUAAAAFCAYAAACNbyblAAAAHElEQVQI12P4//8/w38GIAXDIBKE0DHxgljNBAAO9TXL0Y4OHwAAAABJRU5ErkJggg==';
        this.gameWidth = game.gameWidth;
        this.gameHeight = game.gameHeight;
        this.game = game;
        this.size = 16;
        this.reset();
    }

    reset() {
        this.speed = { x: 4, y: -4 };
        this.position = { x: this.gameWidth / 2, y: this.gameHeight - 40 };
    }

    draw(ctx) {
        ctx.drawImage(
            this.image,
            this.position.x,
            this.position.y,
            this.size,
            this.size
        );
    }

    update(deltaTime) {
        this.position.x += this.speed.x;
        this.position.y += this.speed.y;

        if (
            this.position.x + this.size > this.gameWidth ||
            this.position.x < 0
        ) {
            this.speed.x = -this.speed.x;
        }

        if (this.position.y < 0) {
            this.speed.y = -this.speed.y;
        }

        if (this.position.y + this.size > this.gameHeight) {
            this.game.lives--;
            this.reset();
        }

        let bottomOfBall = this.position.y + this.size;
        let topOfPaddle = this.game.paddle.position.y;
        let leftSideOfPaddle = this.game.paddle.position.x;
        let rightSideOfPaddle = this.game.paddle.position.x + this.game.paddle.width;

        if (
            bottomOfBall >= topOfPaddle &&
            this.position.x >= leftSideOfPaddle &&
            this.position.x + this.size <= rightSideOfPaddle
        ) {
            this.speed.y = -this.speed.y;
            this.position.y = this.game.paddle.position.y - this.size;
        }
    }
}

class Brick {
    constructor(game, position) {
        this.image = document.createElement('img');
        this.image.src = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAnSURBVHgB7cwxEQAACAOxgn8fHB7wBxLMc5MAAAAAAAAAAAAAwEsDKvEAP7OfLYsAAAAASUVORK5CYII=';
        this.game = game;
        this.position = position;
        this.width = 80;
        this.height = 24;
        this.markedForDeletion = false;
    }

    update() {
        if (this.game.ball.position.y <= this.position.y + this.height &&
            this.game.ball.position.y + this.game.ball.size >= this.position.y &&
            this.game.ball.position.x + this.game.ball.size >= this.position.x &&
            this.game.ball.position.x <= this.position.x + this.width) {
            this.game.ball.speed.y = -this.game.ball.speed.y;
            this.markedForDeletion = true;
            this.game.score += 10;
        }
    }

    draw(ctx) {
        ctx.drawImage(
            this.image,
            this.position.x,
            this.position.y,
            this.width,
            this.height
        );
    }
}