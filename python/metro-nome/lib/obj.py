from enum import Enum

class Obj(Enum):
    EMPTY=1
    RAIL_BLUE=2
    RAIL_RED=3
    RAIL_GREEN=4
    RAIL_YELLOW=5
    RAIL_MAGENTA=6
    TRAIN_BLUE=7
    TRAIN_BLUE_HEAD=8
    TRAIN_RED=9
    TRAIN_RED_HEAD=10
    TRAIN_GREEN=11
    TRAIN_GREEN_HEAD=12
    TRAIN_YELLOW=13
    TRAIN_YELLOW_HEAD=14
    TRAIN_MAGENTA=15
    TRAIN_MAGENTA_HEAD=16
    INTERSECTION=17 # where 2 lines intersect, trains must wait for the other train to pass
    STATION=18
    INTERCHANGE=19 # station that facilitates 2 lines changing
    ACCIDENT=20 # where 2 trains collide
    NEWLINE=21 # meta-character

# used for drawing out the models
model_sheet = {
    Obj.EMPTY: "ascii_*, Fore_white, Back_white",
    Obj.RAIL_BLUE: "ascii_#, Fore_cyan, Back_white",
    Obj.RAIL_RED: "ascii_#, Fore_red, Back_white",
    Obj.RAIL_GREEN: "ascii_#, Fore_green, Back_white",
    Obj.RAIL_YELLOW: "ascii_#, Fore_yellow, Back_white",
    Obj.RAIL_MAGENTA: "ascii_#, Fore_magenta, Back_white",
    Obj.TRAIN_BLUE: "ascii_T, Fore_blue, Back_white",
    Obj.TRAIN_BLUE_HEAD: "ascii_T, Fore_blue, Back_white",
    Obj.TRAIN_RED: "ascii_T, Fore_red, Back_white",
    Obj.TRAIN_RED_HEAD: "ascii_T, Fore_red, Back_white",
    Obj.TRAIN_GREEN: "ascii_T, Fore_green, Back_white",
    Obj.TRAIN_GREEN_HEAD: "ascii_T, Fore_green, Back_white",
    Obj.TRAIN_YELLOW: "ascii_T, Fore_yellow, Back_white",
    Obj.TRAIN_YELLOW_HEAD: "ascii_T, Fore_yellow, Back_white",
    Obj.TRAIN_MAGENTA: "ascii_T, Fore_magenta, Back_white",
    Obj.TRAIN_MAGENTA_HEAD: "ascii_T, Fore_magenta, Back_white",
    Obj.INTERSECTION: "ascii_&, Fore_black, Back_white",
    Obj.STATION: "ascii_S, Fore_black, Back_white",
    Obj.INTERCHANGE: "ascii_I, Fore_black, Back_white",
    Obj.ACCIDENT: "ascii_!, Fore_red, Back_yellow",
    Obj.NEWLINE: "ascii_\n"
}

# used for parsing the models from the txt file
reference_sheet ={
    Obj.EMPTY: "*",
    Obj.RAIL_BLUE: "b",
    Obj.RAIL_RED: "r",
    Obj.RAIL_GREEN: "g",
    Obj.RAIL_YELLOW: "y",
    Obj.RAIL_MAGENTA: "m",
    Obj.TRAIN_BLUE: "B",
    Obj.TRAIN_BLUE_HEAD: "1",
    Obj.TRAIN_RED: "R",
    Obj.TRAIN_RED_HEAD: "2",
    Obj.TRAIN_GREEN: "G",
    Obj.TRAIN_GREEN_HEAD: "3",
    Obj.TRAIN_YELLOW: "Y",
    Obj.TRAIN_YELLOW_HEAD: "4",
    Obj.TRAIN_MAGENTA: "M",
    Obj.TRAIN_MAGENTA_HEAD: "5",
    Obj.INTERSECTION: "&",
    Obj.STATION: "S",
    Obj.INTERCHANGE: "I",
    Obj.ACCIDENT: "!", # this should never be invoked in a txt file (in other words, accidents cannot be a planned state)
    Obj.NEWLINE: "\n" # meta character that should never be called by default
}