# helper functions

import obj

# returns a tuple of ascii model, foreground color and background color of a given Obj enum
def read_model_sheet(model_sheet, current_obj):
    return [item.split("_")[1] for item in model_sheet[current_obj].split(",")]

# reads a txt file and returns a dictionary where key is [x:int,y:int] and value is Obj enum
def read_txt_file(reference_sheet, file_name):
    y = 0
    input_grid = {}
    inverted_reference_sheet = {value: key for key, value in reference_sheet.items()}
    fhand = open(file_name, "r")
    for line in fhand:
        x = 0
        for char in line:
            input_grid[x,y] = inverted_reference_sheet[char]
            x += 1
        y += 1
    fhand.close()
    return input_grid