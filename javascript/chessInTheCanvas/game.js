class ChessGame {
    constructor(canvasId) {
        this.canvas = document.getElementById(canvasId);
        this.ctx = this.canvas.getContext('2d');
        this.squareSize = this.canvas.width / 8;
        this.board = this.initializeBoard();
        this.currentPlayer = 'white';
        this.selectedPiece = null;
        this.gameStatus = '';

        this.canvas.addEventListener('click', this.handleClick.bind(this));
        document.getElementById('resetButton').addEventListener('click', this.resetGame.bind(this));

        this.draw();
    }

    initializeBoard() {
        const board = Array(8).fill().map(() => Array(8).fill(null));

        for (let i = 0; i < 8; i++) {
            board[1][i] = new Piece('black', 'pawn', i, 1);
            board[6][i] = new Piece('white', 'pawn', i, 6);
        }

        const pieceOrder = ['rook', 'knight', 'bishop', 'queen', 'king', 'bishop', 'knight', 'rook'];
        for (let i = 0; i < 8; i++) {
            board[0][i] = new Piece('black', pieceOrder[i], i, 0);
            board[7][i] = new Piece('white', pieceOrder[i], i, 7);
        }

        return board;
    }

    draw() {
        for (let y = 0; y < 8; y++) {
            for (let x = 0; x < 8; x++) {
                this.ctx.fillStyle = (x + y) % 2 === 0 ? '#f0d9b5' : '#b58863';
                this.ctx.fillRect(x * this.squareSize, y * this.squareSize, this.squareSize, this.squareSize);
            }
        }

        for (let y = 0; y < 8; y++) {
            for (let x = 0; x < 8; x++) {
                if (this.board[y][x]) {
                    this.board[y][x].draw(this.ctx, this.squareSize);
                }
            }
        }

        if (this.selectedPiece) {
            const validMoves = this.selectedPiece.getValidMoves(this.board);
            this.ctx.fillStyle = 'rgba(255, 255, 0, 0.5)';
            this.ctx.fillRect(this.selectedPiece.x * this.squareSize, this.selectedPiece.y * this.squareSize, this.squareSize, this.squareSize);

            for (let [x, y] of validMoves) {
                this.ctx.beginPath();
                this.ctx.arc(x * this.squareSize + this.squareSize / 2, y * this.squareSize + this.squareSize / 2, this.squareSize / 4, 0, 2 * Math.PI);
                this.ctx.fillStyle = 'rgba(0, 255, 0, 0.3)';
                this.ctx.fill();
            }
        }
    }

    handleClick(event) {
        const rect = this.canvas.getBoundingClientRect();
        const x = Math.floor((event.clientX - rect.left) / this.squareSize);
        const y = Math.floor((event.clientY - rect.top) / this.squareSize);

        if (this.selectedPiece) {
            const validMoves = this.selectedPiece.getValidMoves(this.board);
            if (validMoves.some(move => move[0] === x && move[1] === y)) {
                this.movePiece(this.selectedPiece, x, y);
                this.selectedPiece = null;
                this.currentPlayer = this.currentPlayer === 'white' ? 'black' : 'white';
                document.getElementById('currentPlayer').textContent = `Current Player: ${this.currentPlayer.charAt(0).toUpperCase() + this.currentPlayer.slice(1)}`;
                this.checkGameStatus();
            } else {
                this.selectedPiece = null;
            }
        } else {
            const piece = this.board[y][x];
            if (piece && piece.color === this.currentPlayer) {
                this.selectedPiece = piece;
            }
        }

        this.draw();
    }

    movePiece(piece, x, y) {
        this.board[piece.y][piece.x] = null;
        this.board[y][x] = piece;
        piece.x = x;
        piece.y = y;
        piece.hasMoved = true;
    }

    checkGameStatus() {
        const kings = {white: null, black: null};
        for (let y = 0; y < 8; y++) {
            for (let x = 0; x < 8; x++) {
                const piece = this.board[y][x];
                if (piece && piece.type === 'king') {
                    kings[piece.color] = piece;
                }
            }
        }

        if (!kings.white) {
            this.gameStatus = 'Black wins!';
        } else if (!kings.black) {
            this.gameStatus = 'White wins!';
        } else {
            let hasValidMove = false;
            for (let y = 0; y < 8; y++) {
                for (let x = 0; x < 8; x++) {
                    const piece = this.board[y][x];
                    if (piece && piece.color === this.currentPlayer) {
                        if (piece.getValidMoves(this.board).length > 0) {
                            hasValidMove = true;
                            break;
                        }
                    }
                }
                if (hasValidMove) break;
            }

            if (!hasValidMove) {
                if (this.isKingInCheck(this.currentPlayer)) {
                    this.gameStatus = `${this.currentPlayer === 'white' ? 'Black' : 'White'} wins by checkmate!`;
                } else {
                    this.gameStatus = 'Stalemate!';
                }
            }
        }

        document.getElementById('gameStatus').textContent = this.gameStatus;
    }

    isKingInCheck(color) {
        const king = this.findKing(color);
        const opponentColor = color === 'white' ? 'black' : 'white';

        for (let y = 0; y < 8; y++) {
            for (let x = 0; x < 8; x++) {
                const piece = this.board[y][x];
                if (piece && piece.color === opponentColor) {
                    const moves = piece.getValidMoves(this.board);
                    if (moves.some(move => move[0] === king.x && move[1] === king.y)) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    findKing(color) {
        for (let y = 0; y < 8; y++) {
            for (let x = 0; x < 8; x++) {
                const piece = this.board[y][x];
                if (piece && piece.type === 'king' && piece.color === color) {
                    return piece;
                }
            }
        }
    }

    resetGame() {
        this.board = this.initializeBoard();
        this.currentPlayer = 'white';
        this.selectedPiece = null;
        this.gameStatus = '';
        document.getElementById('currentPlayer').textContent = 'Current Player: White';
        document.getElementById('gameStatus').textContent = '';
        this.draw();
    }
}

const game = new ChessGame('chessBoard');