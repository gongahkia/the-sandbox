import socket
import json
import chess

HOST = "127.0.0.1"  # Localhost
PORT = 65433  # Port for client-server communication

clients = []


def handle_client(client_socket, client_id):
    try:
        board = chess.Board()

        while True:
            data = client_socket.recv(1024)
            if not data:
                break

            # The server can process moves here (currently we don't process them)
            board_fen = board.fen()

            # Convert FEN to compact 64-character format (no slashes or numbers)
            compact_fen = convert_fen_to_compact(board_fen)

            # Send the compact FEN string back to the client
            # Send board and which turn it is
            response = {
                "board": compact_fen,
                "turn": board.turn,  # 0 for white, 1 for black
            }

            client_socket.sendall(json.dumps(response).encode())

    except Exception as e:
        print(f"Error: {e}")
    finally:
        client_socket.close()
        print(f"Client {client_id} disconnected")


def start_server():
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.bind((HOST, PORT))  # Bind to the host and port
    server_socket.listen(2)
    print(f"Server listening on {HOST}:{PORT}")

    client_id = 0

    while True:
        client_socket, _ = server_socket.accept()
        print(f"Client {client_id} connected.")

        clients.append(client_socket)

        client_socket.sendall(
            json.dumps({"player": "white" if client_id % 2 == 0 else "black"}).encode()
        )

        # Handle the client in a separate thread or sequentially
        handle_client(client_socket, client_id)

        client_id += 1


def convert_fen_to_compact(fen):
    """
    Converts a FEN string to a 64-character compact format by removing slashes and
    replacing the numbers with the correct number of empty squares.
    """
    compact = []
    for part in fen.split(" ")[0].split("/"):
        for char in part:
            if char.isdigit():
                compact.append(
                    "." * int(char)
                )  # Replace number with that many empty squares
            else:
                compact.append(char)
    return "".join(compact)


if __name__ == "__main__":
    start_server()
