import socket
import json
import sys
import pygame
from display import init_display, render, quit_display

HOST_IP_ADDRESS = "127.0.0.1"
PORT = 65433
FRAME_RATE = 15  # set 30 for smooth fps


def main():
    try:
        client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        client_socket.connect((HOST_IP_ADDRESS, PORT))  # connect to the server
        print(f"Connected to server at {HOST_IP_ADDRESS}:{PORT}")

        # Receive player color
        data = client_socket.recv(1024)
        player_data = json.loads(data.decode())
        player_color = player_data.get("player", "white")
        print(f"Playing as {player_color}")

        screen, clock = init_display()

        running = True

        while running:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False

            # Receive the current board FEN from the server
            data = client_socket.recv(
                1024
            )  # Receive game state (FEN string) from the server

            if not data:
                print("No data received from server")
                break

            # Make sure the FEN string is valid
            board_info = json.loads(data.decode())
            board_fen = board_info.get("board", "")
            if len(board_fen) != 64:
                print(f"Error: Invalid FEN string received: {board_fen}")
                break

            # Render the game state with the updated board
            render(screen, board_fen, clock, player_color)

            clock.tick(FRAME_RATE)

    except Exception as e:
        print(f"Error: {e}")
    finally:
        print("Closing connection.")
        client_socket.close()
        quit_display()
        sys.exit()


if __name__ == "__main__":
    main()
