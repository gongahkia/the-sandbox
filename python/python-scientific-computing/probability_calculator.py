import random

class Hat:

    #2 aestrix can be used to denote variable number of arguments in a dictionary format
    def __init__ (self, **balls):
        self.contents = []
        print ('Hat has been instantialised!')
        for color, number in balls.items():
            for itervar in range (number):
                self.contents.append (color)
        

    def draw (self, numballs:int):
        duplicate = self.contents
        output =[]
        boi = ''
        if numballs > len (duplicate):
            random.shuffle (duplicate)
            return duplicate
        for itervar in range (numballs):
            boi = random.choice(duplicate)
            duplicate.remove (boi)
            output.append (boi)
        return output
            

def experiment (hat, expected_balls: dict, num_balls_drawn:int, num_experiments:int):
    #print (f'Expected result: {expected_balls}')
    N = num_experiments
    M = 0
    for itervar in range(num_experiments):
        temp_dict = {}
        yes = hat.draw (num_balls_drawn)
        for item in yes:
            temp_dict [item] = temp_dict.get (item, 0) + 1
        #print (f'Draw {itervar + 1} results: {temp_dict}')
        for color in expected_balls:
            if color in temp_dict:
                if expected_balls [color] <= temp_dict [color]:
                    #print ('Im correct!')
                    pass
                
                else:
                    #print ('Number of expected colors incorrect')
                    M = M + 1
                    break

            else:
                #print ('Color not in draw pile')
                M = M + 1
                break

            #print ('This draw was correct!')

        #print (f'M: {M}')
    
    M = N - M
    probability = M/N
    return f'Probability: {probability}%'
        


        
        




