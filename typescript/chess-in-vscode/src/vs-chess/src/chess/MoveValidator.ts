import { ChessBoard } from './ChessBoard';

export class MoveValidator {
  private board: ChessBoard;

  constructor(board: ChessBoard) {
    this.board = board;
  }

  isValidMove(fromRow: number, fromCol: number, toRow: number, toCol: number): boolean {
    const piece = this.board.getPiece(fromRow, fromCol);
    
    if (piece === ' ') return false; // No piece at the starting position
    
    // Check if the move is within the board
    if (toRow < 0 || toRow > 7 || toCol < 0 || toCol > 7) return false;
    
    // Check if the destination is not occupied by a piece of the same color
    const destPiece = this.board.getPiece(toRow, toCol);
    if (this.isSameColor(piece, destPiece)) return false;

    // Validate move based on piece type
    switch (piece.toLowerCase()) {
      case 'p': return this.isValidPawnMove(fromRow, fromCol, toRow, toCol, piece);
      case 'r': return this.isValidRookMove(fromRow, fromCol, toRow, toCol);
      case 'n': return this.isValidKnightMove(fromRow, fromCol, toRow, toCol);
      case 'b': return this.isValidBishopMove(fromRow, fromCol, toRow, toCol);
      case 'q': return this.isValidQueenMove(fromRow, fromCol, toRow, toCol);
      case 'k': return this.isValidKingMove(fromRow, fromCol, toRow, toCol);
      default: return false;
    }
  }

  private isSameColor(piece1: string, piece2: string): boolean {
    return (piece1.toUpperCase() === piece1) === (piece2.toUpperCase() === piece2);
  }

  private isValidPawnMove(fromRow: number, fromCol: number, toRow: number, toCol: number, piece: string): boolean {
    const direction = piece === 'P' ? -1 : 1;
    const startRow = piece === 'P' ? 6 : 1;

    // Move forward
    if (fromCol === toCol && this.board.getPiece(toRow, toCol) === ' ') {
      if (toRow === fromRow + direction) return true;
      if (fromRow === startRow && toRow === fromRow + 2 * direction && this.board.getPiece(fromRow + direction, fromCol) === ' ') return true;
    }

    // Capture diagonally
    if (Math.abs(fromCol - toCol) === 1 && toRow === fromRow + direction) {
      return this.board.getPiece(toRow, toCol) !== ' ' && !this.isSameColor(piece, this.board.getPiece(toRow, toCol));
    }

    return false;
  }

  private isValidRookMove(fromRow: number, fromCol: number, toRow: number, toCol: number): boolean {
    if (fromRow !== toRow && fromCol !== toCol) return false;
    return this.isPathClear(fromRow, fromCol, toRow, toCol);
  }

  private isValidKnightMove(fromRow: number, fromCol: number, toRow: number, toCol: number): boolean {
    const rowDiff = Math.abs(toRow - fromRow);
    const colDiff = Math.abs(toCol - fromCol);
    return (rowDiff === 2 && colDiff === 1) || (rowDiff === 1 && colDiff === 2);
  }

  private isValidBishopMove(fromRow: number, fromCol: number, toRow: number, toCol: number): boolean {
    if (Math.abs(toRow - fromRow) !== Math.abs(toCol - fromCol)) return false;
    return this.isPathClear(fromRow, fromCol, toRow, toCol);
  }

  private isValidQueenMove(fromRow: number, fromCol: number, toRow: number, toCol: number): boolean {
    return this.isValidRookMove(fromRow, fromCol, toRow, toCol) || this.isValidBishopMove(fromRow, fromCol, toRow, toCol);
  }

  private isValidKingMove(fromRow: number, fromCol: number, toRow: number, toCol: number): boolean {
    return Math.abs(toRow - fromRow) <= 1 && Math.abs(toCol - fromCol) <= 1;
  }

  private isPathClear(fromRow: number, fromCol: number, toRow: number, toCol: number): boolean {
    const rowStep = fromRow === toRow ? 0 : (toRow > fromRow ? 1 : -1);
    const colStep = fromCol === toCol ? 0 : (toCol > fromCol ? 1 : -1);

    let currentRow = fromRow + rowStep;
    let currentCol = fromCol + colStep;

    while (currentRow !== toRow || currentCol !== toCol) {
      if (this.board.getPiece(currentRow, currentCol) !== ' ') return false;
      currentRow += rowStep;
      currentCol += colStep;
    }

    return true;
  }
}