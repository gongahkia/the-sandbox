class Player {
    constructor(game) {
        this.game = game;
        this.width = 30;
        this.height = 30;
        this.x = this.game.width / 2 - this.width / 2;
        this.y = this.game.height - this.height - 10;
        this.speed = 5;
    }

    moveLeft() {
        this.x = Math.max(0, this.x - this.speed);
    }

    moveRight() {
        this.x = Math.min(this.game.width - this.width, this.x + this.speed);
    }

    draw(ctx) {
        ctx.fillStyle = '#00ff00';
        ctx.fillRect(this.x, this.y, this.width, this.height);
    }

    shoot() {
        if (this.game.bullets.length < 3) {
            this.game.bullets.push(new Bullet(this.game, this.x + this.width / 2, this.y));
        }
    }
}

class Invader {
    constructor(game, x, y) {
        this.game = game;
        this.width = 30;
        this.height = 30;
        this.x = x;
        this.y = y;
    }

    draw(ctx) {
        ctx.fillStyle = '#ff0000';
        ctx.fillRect(this.x, this.y, this.width, this.height);
    }

    update(deltaX, deltaY) {
        this.x += deltaX;
        this.y += deltaY;
    }
}

class Bullet {
    constructor(game, x, y) {
        this.game = game;
        this.x = x;
        this.y = y;
        this.width = 3;
        this.height = 10;
        this.speed = 7;
    }

    update() {
        this.y -= this.speed;
    }

    draw(ctx) {
        ctx.fillStyle = '#ffffff';
        ctx.fillRect(this.x, this.y, this.width, this.height);
    }
}
