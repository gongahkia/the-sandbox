export class ChessBoard {
  private board: string[][];

  constructor() {
    this.board = [
      ["r", "n", "b", "q", "k", "b", "n", "r"],
      ["p", "p", "p", "p", "p", "p", "p", "p"],
      [" ", " ", " ", " ", " ", " ", " ", " "],
      [" ", " ", " ", " ", " ", " ", " ", " "],
      [" ", " ", " ", " ", " ", " ", " ", " "],
      [" ", " ", " ", " ", " ", " ", " ", " "],
      ["P", "P", "P", "P", "P", "P", "P", "P"],
      ["R", "N", "B", "Q", "K", "B", "N", "R"]
    ];
  }

  getPiece(row: number, col: number): string {
    return this.board[row][col];
  }

  setPiece(row: number, col: number, piece: string): void {
    this.board[row][col] = piece;
  }

  getBoard(): string[][] {
    return this.board.map(row => [...row]);
  }