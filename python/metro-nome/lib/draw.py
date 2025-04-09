# handles rendering logic

from colored import Fore, Back, Style

import util

def draw_text(text, fore_color='white', back_color='green'):
    if text == "\n":
        print("\n", end="")
    else:
        fore_color_attr = getattr(Fore, fore_color.upper(), Fore.white)
        back_color_attr = getattr(Back, back_color.upper(), Back.green)
        print(f"{fore_color_attr}{back_color_attr}{text}{Style.reset}", end="")

def draw_grid(input_grid, model_sheet):
    print("\n", end="")
    for coordinate, obj in input_grid.items():
        # print(coordinate)
        draw_text(*util.read_model_sheet(model_sheet, obj))
    print("\n", end="")