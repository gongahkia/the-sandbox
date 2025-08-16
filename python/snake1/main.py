from snake_game import player
from snake_game import cherry
from snake_game import print_room
from snake_game import row_calculation
import time 

def gameplay():
    print ('Welcome to Snake Game.\nInstructions are as follows:\nPress W key to go up\nPress A key to go left\nPress S key to go down\nPress D key to go right')

    s = player ('snake')
    c = cherry ()

    user = input ('Ready to start?\n[Y/N]\n')
    if user.lower() == 'y':
        print ('Game starting in...')
        time.sleep(0.75)
        print ('3')
        time.sleep(0.75)
        print ('2')
        time.sleep(0.75)
        print ('1')
        time.sleep(0.5)
    else:
        print ('Ok. See you again next time!')
        exit()
    
    while s.score < 1000:
        print_room (s,c)
        uip = input ('[W/A/S/D]: ')

    
        if uip.lower() == 'w':
            s.up(1)

        elif uip.lower() == 'a':
            s.left(1)

        elif uip.lower() == 's':
            s.down(1)

        elif uip.lower() == 'd':
            s.right(1)

        else:
            print(f'invalid input.')
    
    endscreen()
        

def endscreen ():
    uip = input(f'Congradulations. You have won!\nPlay again?\n[Y/N]\n')
    if uip.lower() == 'y':
        gameplay()
    else:
        print ('Thanks for playing!')
        exit()
                



