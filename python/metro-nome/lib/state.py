# handles state logic

import obj
import util

# generates an input grid while adhering to all the rules
def generate():
    pass

# updates state of the input grid by one iteration cycle
def update(input_grid):
    updated_input_grid = {}

    for coordinate, element in input_grid.items():

        # fua 
        # update the state of each cell similar to cellular automata logic, right now nothing is updated

        match element:

            case obj.Obj.EMPTY: 
                updated_input_grid[coordinate] = obj.Obj.EMPTY

            case obj.Obj.RAIL_BLUE:
                pass

            case obj.Obj.RAIL_RED: 
                pass

            case obj.Obj.RAIL_GREEN:
                pass

            case obj.Obj.RAIL_YELLOW: 
                pass

            case obj.Obj.RAIL_MAGENTA: 
                pass

            case obj.Obj.TRAIN_BLUE: 
                pass

            case obj.Obj.TRAIN_BLUE_HEAD: 
                pass

            case obj.Obj.TRAIN_RED: 
                pass

            case obj.Obj.TRAIN_RED_HEAD:
                pass

            case obj.Obj.TRAIN_GREEN: 
                pass

            case obj.Obj.TRAIN_GREEN_HEAD: 
                pass

            case obj.Obj.TRAIN_YELLOW: 
                pass

            case obj.Obj.TRAIN_YELLOW_HEAD: 
                pass

            case obj.Obj.TRAIN_MAGENTA: 
                pass

            case obj.Obj.TRAIN_MAGENTA_HEAD: 
                pass

            case obj.Obj.INTERSECTION: 
                pass

            case obj.Obj.STATION: 
                updated_input_grid[coordinate] = obj.Obj.STATION

            case obj.Obj.INTERCHANGE: 
                pass

            case obj.Obj.ACCIDENT:
                pass

            case obj.Obj.NEWLINE: 
                updated_input_grid[coordinate] = obj.Obj.NEWLINE

            case default:
                print("edge case hit, this should not normally run")

    return updated_input_grid