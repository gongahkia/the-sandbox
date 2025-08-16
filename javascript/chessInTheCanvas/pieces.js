class Piece {
    constructor(color, type, x, y) {
        this.color = color;
        this.type = type;
        this.x = x;
        this.y = y;
        this.hasMoved = false;
    }

    draw(ctx, squareSize) {
        const centerX = this.x * squareSize + squareSize / 2;
        const centerY = this.y * squareSize + squareSize / 2;
        ctx.fillStyle = this.color === 'white' ? '#fff' : '#000';
        ctx.strokeStyle = this.color === 'white' ? '#000' : '#fff';
        ctx.lineWidth = 2;
        ctx.beginPath();

        switch (this.type) {
            case 'pawn':
                ctx.moveTo(centerX, centerY + 15);
                ctx.lineTo(centerX - 10, centerY - 5);
                ctx.lineTo(centerX + 10, centerY - 5);
                ctx.closePath();
                break;
            case 'rook':
                ctx.rect(centerX - 10, centerY - 15, 20, 30);
                break;
            case 'knight':
                ctx.moveTo(centerX - 10, centerY + 15);
                ctx.lineTo(centerX - 5, centerY - 15);
                ctx.lineTo(centerX + 15, centerY - 5);
                ctx.lineTo(centerX + 5, centerY + 15);
                ctx.closePath();
                break;
            case 'bishop':
                ctx.moveTo(centerX, centerY - 15);
                ctx.lineTo(centerX - 10, centerY + 15);
                ctx.lineTo(centerX + 10, centerY + 15);
                ctx.closePath();
                break;
            case 'queen':
                ctx.moveTo(centerX, centerY - 15);
                ctx.lineTo(centerX - 15, centerY + 15);
                ctx.lineTo(centerX + 15, centerY + 15);
                ctx.closePath();
                ctx.moveTo(centerX - 7, centerY);
                ctx.lineTo(centerX + 7, centerY);
                break;
            case 'king':
                ctx.moveTo(centerX, centerY - 15);
                ctx.lineTo(centerX - 15, centerY + 15);
                ctx.lineTo(centerX + 15, centerY + 15);
                ctx.closePath();
                ctx.moveTo(centerX, centerY - 5);
                ctx.lineTo(centerX, centerY + 5);
                ctx.moveTo(centerX - 7, centerY);
                ctx.lineTo(centerX + 7, centerY);
                break;
        }

        ctx.fill();
        ctx.stroke();
    }

    getValidMoves(board) {
        let moves = [];
        switch (this.type) {
            case 'pawn':
                moves = this.getPawnMoves(board);
                break;
            case 'rook':
                moves = this.getStraightMoves(board);
                break;
            case 'knight':
                moves = this.getKnightMoves(board);
                break;
            case 'bishop':
                moves = this.getDiagonalMoves(board);
                break;
            case 'queen':
                moves = [...this.getStraightMoves(board), ...this.getDiagonalMoves(board)];
                break;
            case 'king':
                moves = this.getKingMoves(board);
                break;
        }
        return moves;
    }

    getPawnMoves(board) {
        const moves = [];
        const direction = this.color === 'white' ? -1 : 1;
        const startRow = this.color === 'white' ? 6 : 1;

        if (board[this.y + direction][this.x] === null) {
            moves.push([this.x, this.y + direction]);
            if (this.y === startRow && board[this.y + 2 * direction][this.x] === null) {
                moves.push([this.x, this.y + 2 * direction]);
            }
        }

        for (let dx of [-1, 1]) {
            const newX = this.x + dx;
            const newY = this.y + direction;
            if (newX >= 0 && newX < 8 && board[newY][newX] && board[newY][newX].color !== this.color) {
                moves.push([newX, newY]);
            }
        }

        return moves;
    }

    getStraightMoves(board) {
        const moves = [];
        const directions = [[0, 1], [1, 0], [0, -1], [-1, 0]];

        for (let [dx, dy] of directions) {
            let x = this.x + dx;
            let y = this.y + dy;
            while (x >= 0 && x < 8 && y >= 0 && y < 8) {
                if (board[y][x] === null) {
                    moves.push([x, y]);
                } else {
                    if (board[y][x].color !== this.color) {
                        moves.push([x, y]);
                    }
                    break;
                }
                x += dx;
                y += dy;
            }
        }

        return moves;
    }

    getKnightMoves(board) {
        const moves = [];
        const knightMoves = [[2, 1], [1, 2], [-1, 2], [-2, 1], [-2, -1], [-1, -2], [1, -2], [2, -1]];

        for (let [dx, dy] of knightMoves) {
            const newX = this.x + dx;
            const newY = this.y + dy;
            if (newX >= 0 && newX < 8 && newY >= 0 && newY < 8) {
                if (board[newY][newX] === null || board[newY][newX].color !== this.color) {
                    moves.push([newX, newY]);
                }
            }
        }

        return moves;
    }

    getDiagonalMoves(board) {
        const moves = [];
        const directions = [[1, 1], [1, -1], [-1, 1], [-1, -1]];

        for (let [dx, dy] of directions) {
            let x = this.x + dx;
            let y = this.y + dy;
            while (x >= 0 && x < 8 && y >= 0 && y < 8) {
                if (board[y][x] === null) {
                    moves.push([x, y]);
                } else {
                    if (board[y][x].color !== this.color) {
                        moves.push([x, y]);
                    }
                    break;
                }
                x += dx;
                y += dy;
            }
        }

        return moves;
    }

    getKingMoves(board) {
        const moves = [];
        const directions = [[0, 1], [1, 1], [1, 0], [1, -1], [0, -1], [-1, -1], [-1, 0], [-1, 1]];

        for (let [dx, dy] of directions) {
            const newX = this.x + dx;
            const newY = this.y + dy;
            if (newX >= 0 && newX < 8 && newY >= 0 && newY < 8) {
                if (board[newY][newX] === null || board[newY][newX].color !== this.color) {
                    moves.push([newX, newY]);
                }
            }
        }

        return moves;
    }
}