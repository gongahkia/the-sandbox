# main execution code

# fua
    # back-end in obj.py, implement a checker that assigns a BLUE_TRAIN_HEAD special node to one square so the direction of trains can be specified
    # implement update() in state.py to update the state of the diagram each turn
    # implement main program loop that can also check whether there has been a change in grid state
    # implement a generator that is able to generate random coherent grids

# standard library
import sys
import os
import time

# adds everything within the ../lib directory to Python's module search path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '../lib'))) 

# our own modules
import draw
import obj
import util
import state

# main execution code
if __name__ == '__main__':

    # --- testing ---
    # draw.draw_text(*util.read_model_sheet(obj.model_sheet, obj.Obj.EMPTY))
    # draw.draw_text(*util.read_model_sheet(obj.model_sheet, obj.Obj.RAIL_BLUE))
    # draw.draw_text(*util.read_model_sheet(obj.model_sheet, obj.Obj.RAIL_RED))
    # draw.draw_text(*util.read_model_sheet(obj.model_sheet, obj.Obj.RAIL_GREEN))
    # draw.draw_text(*util.read_model_sheet(obj.model_sheet, obj.Obj.RAIL_YELLOW))
    # draw.draw_text(*util.read_model_sheet(obj.model_sheet, obj.Obj.RAIL_MAGENTA))
    # draw.draw_text(*util.read_model_sheet(obj.model_sheet, obj.Obj.TRAIN_BLUE))
    # draw.draw_text(*util.read_model_sheet(obj.model_sheet, obj.Obj.TRAIN_RED))
    # draw.draw_text(*util.read_model_sheet(obj.model_sheet, obj.Obj.TRAIN_GREEN))
    # draw.draw_text(*util.read_model_sheet(obj.model_sheet, obj.Obj.TRAIN_YELLOW))
    # draw.draw_text(*util.read_model_sheet(obj.model_sheet, obj.Obj.TRAIN_MAGENTA))
    # draw.draw_text(*util.read_model_sheet(obj.model_sheet, obj.Obj.INTERSECTION))
    # draw.draw_text(*util.read_model_sheet(obj.model_sheet, obj.Obj.STATION))
    # draw.draw_text(*util.read_model_sheet(obj.model_sheet, obj.Obj.INTERCHANGE))
    # draw.draw_text(*util.read_model_sheet(obj.model_sheet, obj.Obj.ACCIDENT))

    old_input_grid_local = util.read_txt_file(obj.reference_sheet, "../lib/sample1.txt")
    while True:
        os.system("clear")
        draw.draw_grid(old_input_grid_local, obj.model_sheet)
        old_input_grid_local = state.update(old_input_grid_local)
        time.sleep(1)