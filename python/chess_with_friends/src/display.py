import pygame

# Colors
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
LIGHT_BROWN = (222, 184, 135)
DARK_BROWN = (139, 69, 19)


# Initialize Pygame display
def init_display():
    pygame.init()
    screen = pygame.display.set_mode((480, 480))  # 8x8 chessboard
    pygame.display.set_caption("Chess Game")
    clock = pygame.time.Clock()
    return screen, clock


def quit_display():
    pygame.quit()


# Draw the chessboard with letters for pieces
def render(screen, board_fen, clock, player_color):
    """
    Render the chessboard and the pieces as letters
    """
    screen.fill(WHITE)

    # Draw the chessboard
    tile_size = 60  # Each tile is 60x60 px
    for row in range(8):
        for col in range(8):
            rect = pygame.Rect(col * tile_size, row * tile_size, tile_size, tile_size)
            color = LIGHT_BROWN if (row + col) % 2 == 0 else DARK_BROWN
            pygame.draw.rect(screen, color, rect)

            # Get the piece at the current board position
            piece = board_fen[row * 8 + col]
            if piece != ".":
                # If there is a piece, display it as a letter
                font = pygame.font.Font(None, 48)  # Use a font for the pieces
                text_color = WHITE if piece.isupper() else BLACK
                text_surface = font.render(piece, True, text_color)
                text_rect = text_surface.get_rect(center=rect.center)
                screen.blit(text_surface, text_rect)

    pygame.display.flip()  # Update the display
