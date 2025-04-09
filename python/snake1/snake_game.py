import random

class player:

    def __init__ (self, name):
        self.name = name
        self.location = [0,0]
        self.score = 0
        self.health = 2
        #print (f'{self.name} was instantiated')

    def up (self, num:int):
        for itervar in range(num):
            self.location[1] += 1

    def down (self, num:int):
        for itervar in range(num):
            self.location[1] -= 1

    def left (self, num:int):
        for itervar in range(num):
            self.location[0] -= 1

    def right (self, num:int):
        for itervar in range(num):
            self.location[0] += 1

class cherry:
    
    def __init__ (self):
        #print ('cherry was instantiated')
        self.location = [5,2]
        
    def randomise (self):
        self.location = [random.randint(-7,7), random.randint(-3,4)] 

def row_calculation (name, cherry):
    templist = []
    rownum = name.location [1]
    colnum = name.location [0]
    rowoffset = -(rownum) + 5
    coloffset = colnum + 9

    c_rownum = cherry.location [1]
    c_colnum = cherry.location [0]
    c_rowoffset = -(c_rownum) + 5
    c_coloffset = c_colnum + 9

    print (f'snake:{coloffset},{rowoffset}, cherry:{c_coloffset},{c_rowoffset}')

    for rows in range(9):
        toprint = ''
        for columns in range(19):
            if rowoffset > 8:
                rowoffset = rowoffset - 8
            if rowoffset < 0:
                rowoffset = rowoffset + 8
            if coloffset > 18:
                coloffset = coloffset - 18
            if coloffset < 0:
                coloffset = coloffset + 18
            if coloffset == c_coloffset:
                if rowoffset == c_rowoffset:
                    name.score = name.score + 1
                    cherry.randomise()
                    pass
            if columns == coloffset:
                if rows == rowoffset:
                    toprint += '0'
                else:
                    toprint += ' '
            if columns == c_coloffset:
                if rows == c_rowoffset:
                    toprint += '*'
                else:
                    toprint += ' '

            else:
                toprint += ' '
        templist.append(toprint)   
    return templist

def print_room (name, cherry):
    storage = []
    permstorage = {}

    print (' ____________________')
    #header of room

    printlist = row_calculation (name, cherry)

    for itervar in range(len(printlist)):
        print(f'|{printlist[itervar]}|\n', end= '')    
    #body of room: each line comprises 19 spaces
    
    print (' ____________________')

    print (f'\n       SCORE: {name.score}')
    #base of room



