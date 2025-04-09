# ----- required imports -----

import pygame
import sys

# ----- helper functions -----


def load_spritesheet(filepath):
    """
    attempts to load the spritesheet
    """
    try:
        spritesheet = pygame.image.load(filepath).convert_alpha()
        return (True, spritesheet)
    except pygame.error as e:
        print(f"Error: Unable to load spritesheet: {e}")
        return (False, None)


def display_spritesheet_debug(spritesheet_filepath):
    """
    for rapid debugging of viewing the spritesheet
    """
    try:
        pygame.init()
        screen = pygame.display.set_mode((800, 600))
        pygame.display.set_caption("Display spritesheet (debug)")
        spritesheet = load_spritesheet(spritesheet_filepath)
        running = True
        while running:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False
            screen.fill((255, 255, 255))
            screen.blit(spritesheet, (0, 0))
            pygame.display.flip()
        pygame.quit()
        return True
    except:
        return False


def get_scaled_sprite(spritesheet, sprite_rect, scale_factor):
    """
    extract and scale a sprite from the spritesheet
    """
    sprite = spritesheet.subsurface(sprite_rect)
    scaled_sprite = pygame.transform.scale(
        sprite, (sprite_rect.width * scale_factor, sprite_rect.height * scale_factor)
    )
    return scaled_sprite


def iso_coords(x, y, z, screen_width, screen_height, block_size):
    """
    convert cartesian coordinates to isometric coordinates
    """
    iso_x = (x - y) * (block_size // 2)
    iso_y = (x + y) * (block_size // 4) - z * block_size // 2
    return iso_x + screen_width // 2, iso_y + screen_height // 2


def render_blocks(
    screen, blocks_dict, color_map, block_size, screen_width, screen_height
):
    """
    render blocks based on the provided dictionary
    """
    sorted_blocks = sorted(
        blocks_dict.items(), key=lambda item: item[0][2]
    )  # sort blocks by Z value for rendering
    for (x, y, z), block_type in sorted_blocks:
        color = color_map.get(
            block_type, (255, 255, 255)
        )  # default to white if unknown block type registered
        iso_x, iso_y = iso_coords(x, y, z, screen_width, screen_height, block_size)
        pygame.draw.rect(
            screen, color, (iso_x, iso_y - block_size // 2, block_size, block_size)
        )  # draw each block as a rectangle


def render_player(screen, player_position, color_map, block_size):
    """
    renders the player in an isometric world
    """
    try:
        iso_x, iso_y = iso_coords(player_position[0], player_position[1])
        pygame.draw.rect(
            screen,
            color_map["player"],
            (
                iso_x + block_size // 4,
                iso_y - block_size // 2 + block_size // 4,
                block_size // 2,
                block_size // 2,
            ),
        )
        return True
    except:
        return False


def setup_window(screen_width, screen_height, screen_caption):
    """
    attempts to set up the pygame window
    """
    try:
        pygame.init()
        screen = pygame.display.set_mode((screen_width, screen_height))
        pygame.display.set_caption(screen_caption)
        return (True, screen)
    except:
        return (False, None)


def main(
    screen_width, screen_height, screen_caption, blocks_dict, color_map, block_size
):
    """
    main function to run the event loop
    """
    screen_tuple = setup_window(screen_width, screen_height, screen_caption)
    if screen_tuple[0]:
        screen = screen_tuple[1]
        while True:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    pygame.quit()
                    sys.exit()
            screen.fill((135, 206, 235))  # fills the background with a light sky color
            render_blocks(
                screen, blocks_dict, color_map, block_size, screen_width, screen_height
            )
            pygame.display.flip()
    else:
        print("Error: Unable to render pygame screen")
