# ----- required imports -----

import helper as h

# ----- sample execution code -----

if __name__ == "__main__":

    SCREEN_WIDTH = 800
    SCREEN_HEIGHT = 600
    SCREEN_CAPTION = "gohan (debug)"
    BLOCK_SIZE = 64

    COLOR_MAP = {
        "player": (255, 0, 255),
        "grass": (0, 255, 0),
        "dirt": (139, 69, 19),
        "stone": (128, 128, 128),
        "sand": (194, 178, 128),
        "water": (0, 0, 255),
        "lava": (255, 165, 0),
        "leaves": (34, 139, 34),
        "oak_log": (139, 69, 19),
    }

    BLOCKS_DICT = {
        (0, 0, 0): "grass",
        (1, 0, 0): "grass",
        (2, 0, 0): "grass",
        (3, 0, 0): "grass",
        (4, 0, 0): "grass",
        (5, 0, 0): "grass",
        (6, 0, 0): "grass",
        (7, 0, 0): "grass",
        (8, 0, 0): "grass",
        (9, 0, 0): "grass",
        (10, 0, 0): "grass",
        (11, 0, 0): "grass",
        (0, 1, 0): "grass",
        (1, 1, 0): "grass",
        (2, 1, 0): "water",
        (3, 1, 0): "water",
        (4, 1, 0): "water",
        (5, 1, 0): "water",
        (6, 1, 0): "water",
        (7, 1, 0): "water",
        (8, 1, 0): "water",
        (9, 1, 0): "grass",
        (10, 1, 0): "grass",
        (11, 1, 0): "grass",
        (0, 2, 0): "grass",
        (1, 2, 0): "grass",
        (2, 2, 0): "water",
        (3, 2, 0): "water",
        (4, 2, 0): "water",
        (5, 2, 0): "water",
        (6, 2, 0): "water",
        (7, 2, 0): "water",
        (8, 2, 0): "water",
        (9, 2, 0): "grass",
        (10, 2, 0): "grass",
        (11, 2, 0): "grass",
        (0, 3, 0): "grass",
        (1, 3, 0): "grass",
        (2, 3, 0): "water",
        (3, 3, 0): "water",
        (4, 3, 0): "water",
        (5, 3, 0): "water",
        (6, 3, 0): "water",
        (7, 3, 0): "water",
        (8, 3, 0): "water",
        (9, 3, 0): "grass",
        (10, 3, 0): "grass",
        (11, 3, 0): "grass",
        (0, 4, 0): "grass",
        (1, 4, 0): "grass",
        (2, 4, 0): "grass",
        (3, 4, 0): "grass",
        (4, 4, 0): "grass",
        (5, 4, 0): "grass",
        (6, 4, 0): "grass",
        (7, 4, 0): "grass",
        (8, 4, 0): "grass",
        (9, 4, 0): "grass",
        (10, 4, 0): "grass",
        (11, 4, 0): "grass",
    }

    # BLOCKS_DICT = {
    #     (0, 0, 0): "grass", (1, 0, 0): "grass", (2, 0, 0): "grass", (3, 0, 0): "grass",
    #     (4, 0, 0): "grass", (5, 0, 0): "grass", (6, 0, 0): "grass", (7, 0, 0): "grass",
    #     (8, 0, 0): "grass", (9, 0, 0): "grass", (10, 0, 0): "grass", (11, 0, 0): "grass",
    #     (0, 1, 0): "grass", (1, 1, 0): "stone", (2, 1, 0): "stone", (3, 1, 0): "lava",
    #     (4, 1, 0): "stone", (5, 1, 0): "grass", (6, 1, 0): "stone", (7, 1, 0): "stone",
    #     (8, 1, 0): "grass", (9, 1, 0): "grass", (10, 1, 0): "grass", (11, 1, 0): "grass",
    #     (0, 2, 0): "grass", (1, 2, 0): "stone", (2, 2, 0): "stone", (3, 2, 0): "stone",
    #     (4, 2, 0): "stone", (5, 2, 0): "lava",   (6, 2, 0): "stone", (7, 2 ,0) : "stone",
    #     (8 ,2 ,0) : "grass" ,(9 ,2 ,0) : "grass" ,(10 ,2 ,0) : "grass" ,(11 ,2 ,0) : "grass",
    #     (0 ,3 ,0) : "grass" ,(1 ,3 ,0) : "stone" ,(2 ,3 ,0) : "stone" , (3 ,3 ,0) : "lava" ,
    #     (4 ,3 ,0) : "stone" , (5 ,3 ,0) : "grass" , (6 ,3 ,0) : "stone" , (7 ,3 ,0) : "stone" ,
    #     (8 ,3 ,0) : "grass" , (9 ,3 ,0) : "grass" , (10 ,3 ,0) : "grass" , (11 ,3 ,0) : "grass",
    # }

    h.main(
        SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_CAPTION, BLOCKS_DICT, COLOR_MAP, BLOCK_SIZE
    )
