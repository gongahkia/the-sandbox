import * as vscode from 'vscode';
import { ChessGame } from './chess/ChessGame';

export function activate(context: vscode.ExtensionContext) {
  let disposable = vscode.commands.registerCommand('extension.startChessGame', () => {
    ChessboardPanel.createOrShow(context.extensionUri);
  });

  context.subscriptions.push(disposable);
}

class ChessboardPanel {
  public static currentPanel: ChessboardPanel | undefined;
  private readonly _panel: vscode.WebviewPanel;
  private _disposables: vscode.Disposable[] = [];

  private constructor(panel: vscode.WebviewPanel, extensionUri: vscode.Uri) {
    this._panel = panel;
    this._panel.onDidDispose(() => this.dispose(), null, this._disposables);
    this._panel.webview.html = this._getWebviewContent(extensionUri);
  }

  public static createOrShow(extensionUri: vscode.Uri) {
    if (ChessboardPanel.currentPanel) {
      ChessboardPanel.currentPanel._panel.reveal(vscode.ViewColumn.Beside);
    } else {
      const panel = vscode.window.createWebviewPanel(
        'chessboard',
        'Chess Game',
        vscode.ViewColumn.Beside,
        {
          enableScripts: true
        }
      );
      ChessboardPanel.currentPanel = new ChessboardPanel(panel, extensionUri);
    }
  }

  private _getWebviewContent(extensionUri: vscode.Uri): string {
    const scriptUri = vscode.Uri.joinPath(extensionUri, 'media', 'chessboard.js');
    const styleUri = vscode.Uri.joinPath(extensionUri, 'media', 'chessboard.css');

    return `<!DOCTYPE html>
  <html lang="en">
  <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>Chess Game</title>
      <link href="${styleUri}" rel="stylesheet">
  </head>
  <body>
      <div id="chessboard" class="chessboard"></div>
      <div id="status"></div>
      <button id="newGameBtn">New Game</button>
      <script src="${scriptUri}"></script>
      <script>
          (function() {
              const vscode = acquireVsCodeApi();
              const board = document.getElementById('chessboard');
              const status = document.getElementById('status');
              const newGameBtn = document.getElementById('newGameBtn');

              let selectedPiece = null;

              function createBoard() {
                  board.innerHTML = '';
                  for (let i = 0; i < 8; i++) {
                      for (let j = 0; j < 8; j++) {
                          const square = document.createElement('div');
                          square.className = 'square ' + ((i + j) % 2 === 0 ? 'light' : 'dark');
                          square.dataset.row = i;
                          square.dataset.col = j;
                          square.addEventListener('click', handleSquareClick);
                          board.appendChild(square);
                      }
                  }
              }

              function handleSquareClick(event) {
                  const square = event.target;
                  const row = parseInt(square.dataset.row);
                  const col = parseInt(square.dataset.col);

                  if (selectedPiece) {
                      // Attempt to move the selected piece
                      vscode.postMessage({
                          command: 'move',
                          from: selectedPiece,
                          to: { row, col }
                      });
                      selectedPiece = null;
                  } else if (square.hasChildNodes()) {
                      // Select the piece
                      selectedPiece = { row, col };
                      square.classList.add('selected');
                  }
              }

              function updateBoard(boardState) {
                  const squares = board.getElementsByClassName('square');
                  for (let i = 0; i < 64; i++) {
                      const square = squares[i];
                      const row = Math.floor(i / 8);
                      const col = i % 8;
                      const piece = boardState[row][col];
                      square.innerHTML = piece !== ' ' ? getPieceUnicode(piece) : '';
                  }
              }

              function getPieceUnicode(piece) {
                  const pieceUnicode = {
                      'K': '♔', 'Q': '♕', 'R': '♖', 'B': '♗', 'N': '♘', 'P': '♙',
                      'k': '♚', 'q': '♛', 'r': '♜', 'b': '♝', 'n': '♞', 'p': '♟'
                  };
                  return pieceUnicode[piece] || '';
              }

              newGameBtn.addEventListener('click', () => {
                  vscode.postMessage({ command: 'newGame' });
              });

              // Initial board setup
              createBoard();

              // Listen for messages from the extension
              window.addEventListener('message', event => {
                  const message = event.data;
                  switch (message.command) {
                      case 'updateBoard':
                          updateBoard(message.boardState);
                          break;
                      case 'updateStatus':
                          status.textContent = message.status;
                          break;
                  }
              });

              // Request initial board state
              vscode.postMessage({ command: 'requestBoardState' });
          })();
      </script>
  </body>
  </html>`;
  }

  public dispose() {
    ChessboardPanel.currentPanel = undefined;
    this._panel.dispose();
    while (this._disposables.length) {
      const disposable = this._disposables.pop();
      if (disposable) {
        disposable.dispose();
      }
    }
  }
}