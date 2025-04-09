class Rectangle:

    #run upon the instance of said class object instantializing
    def __init__ (self, width = 0, height = 0):
        self.width = width
        self.height = height
        print (f'Rectangle is instantiated')

    def set_width (self, width:int):
        self.width = width 

    def set_height (self, height:int):
        self.height = height
    
    def get_area (self):
        self.area = self.height * self.width
        return self.area
    
    def get_perimeter (self):
        self.perimeter = (2 * self.width) + (2 * self.height)
        return self.perimeter

    def get_diagonal (self):
        self.diagonal = (((self.width ** 2) + (self.height ** 2)) ** 0.5)
        return self.diagonal
        
    def get_picture (self):
        output = ''
        if self.width > 50 or self.height > 50:
            output = 'Too big for picture'
            pass
        else:
            for num in range (self.height):
                for numlines in range (self.width):
                    output = output + '*'
                output = output + '\n'
        return output

    def get_amount_inside (self, other_shape):

        #in case the other shape is able to fit within the original shape when rotated 90 degrees
        num_heightwise = self.height // other_shape.height
        num_widthwise = self.width // other_shape.width
        num_shapes1 = num_heightwise * num_widthwise

        num_crosswise = self.height // other_shape.width
        num_crosswise2 = self.width // other_shape.height
        num_shapes2 = num_crosswise * num_crosswise2

        if num_shapes1 > num_shapes2:
            output = num_shapes1
        else:
            output = num_shapes2
        return output

    def __repr__ (self):
        return f'Rectangle (width = {self.width}, height = {self.height})'

class Square (Rectangle): 

    #aside from redefining the __init__ constructor method, the Square child class inherits all the other instance methods and instance attributes from its parent class, Rectangle 
    def __init__ (self, side = 0):
        self.side = side
        self.width = side
        self.height = side
        print (f'Square is instantiated!')

    def set_side (self, side: int):
        self.side = side
        self.width = side
        self.height = side 

    #here, the __repr__ method also overrides the previously defined __repr__ method in the Rectangle parent class
    def __repr__ (self):
        return f'Square (side = {self.side})'


