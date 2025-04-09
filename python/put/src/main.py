# FUA
# - handle cursor movement
# - handle scrolling screen
# - handle opening of file from CLI using a `put` command

# - vim time :)
    # - implement modes using enums 
        # use match case statement to determine current mode of editor
"""class mode(Enum):
    Command = 1
    Normal = 2
    Insert = 3
    Visual = 4
"""
# - implement vim key binds
    # 1. COMMAND MODE
        # - to NORMAL MODE with esc
        # - write files with :w enter
        # - write and quit file with :wq enter
        # - quit file with :q
        # - quit file if there was an edit that you dont wanna save with :q!
            # - implement check to see whether a file has been written to
            # - implement command :q that allows quitting of PUT if file has not been written to
            # - implement command for :q that does not allow a new file just created to be quit normally with :q if it is empty
    # 2. NORMAL MODE 
        # to COMMAND MODE with :
        # to INSERT MODE with i, I, a, A, o, O 
        # to VISUAL MODE with v
            # - implement entering insert mode using I, a, A, o, O as well
            # - implement movement using hjklG and w b and the combination of numbers and movement keys as well
    # 3. INSERT MODE
        # to NORMAL MODE with esc
            # - implement any additonal special keys similar to the arrow keys that i need to ignore
    # 4. VISUAL MODE
        # to NORMAL MODE with esc
            # - implement entering visual mode using small v, large V etc 
# - subsequently implementing opening a local file

# ----------

# required imports
import curses

# ----------

def get_file_name(stdscr):
    file_name:str = ""
    curses.echo() # enables input echoing
    while True:
        stdscr.erase()
        stdscr.addstr(0,0,f"Enter file name: {file_name}_")
        stdscr.refresh()
        name_buffer = stdscr.getch()
        if name_buffer == curses.KEY_BACKSPACE or name_buffer == 127: # backspace
            file_name = file_name[:-1]
        elif name_buffer == curses.KEY_ENTER or name_buffer == 10: # enter
            break
        elif name_buffer == ord("\t") or name_buffer == 9: # tab
            file_name += "\t"
        elif name_buffer == curses.KEY_UP or name_buffer == curses.KEY_DOWN or name_buffer == curses.KEY_LEFT or name_buffer == curses.KEY_RIGHT:
            pass
        else:
            file_name += chr(name_buffer)
    return file_name

def main(stdscr):
    file_name:str = "" 
    text_buffer:str = ""
    curses.curs_set(0) # hide cursor

    file_name = get_file_name(stdscr)

    try:
        fhand = open(file_name, "r")
        text_buffer = fhand.read()
        fhand.close()
    except:
        pass

    stdscr.erase()
    stdscr.addstr(0,15,"PUT --> Press <esc> to save changes.")
    stdscr.addstr(2,0,f"{text_buffer}_")
    stdscr.refresh() 

    while True:

        char_buffer = stdscr.getch()

        if char_buffer == 27: # escape quits the editor and saves file as a text file
            file_name_buffer = ""
            if file_name == "":
                stdscr.erase()
                stdscr.addstr(0,15,f"File name: {file_name}_")
                stdscr.addstr(2,0,f"{text_buffer}_")
                stdscr.refresh() 
                while True:
                    file_name_buffer = stdscr.getch()
                    if file_name_buffer == curses.KEY_BACKSPACE or file_name_buffer == 127: # backspace
                        file_name = text_buffer[:-1]
                    elif file_name_buffer == curses.KEY_ENTER or file_name_buffer == 10: # enter
                        break
                    elif file_name_buffer == ord("\t") or file_name_buffer == 9: # tab
                        file_name += "\t"
                    elif file_name_buffer == curses.KEY_UP or file_name_buffer == curses.KEY_DOWN or file_name_buffer == curses.KEY_LEFT or file_name_buffer == curses.KEY_RIGHT:
                        pass
                    else:
                        file_name += chr(file_name_buffer)
                    stdscr.erase()
                    stdscr.addstr(0,15,f"File name: {file_name}_")
                    stdscr.addstr(2,0,f"{text_buffer}_")
                    stdscr.refresh() 
            fhand = open(file_name, "w")
            fhand.write(text_buffer)
            fhand.close()
            break
            
        elif char_buffer == curses.KEY_BACKSPACE or char_buffer == 127: # backspace
            text_buffer = text_buffer[:-1]
        elif char_buffer == curses.KEY_ENTER or char_buffer == 10: # enter
            text_buffer += "\n"
        elif char_buffer == ord("\t") or char_buffer == 9: # tab
            text_buffer += "\t"
        elif char_buffer == curses.KEY_UP or char_buffer == curses.KEY_DOWN or char_buffer == curses.KEY_LEFT or char_buffer == curses.KEY_RIGHT:
            pass
        else:
            text_buffer += chr(char_buffer)

        stdscr.erase()
        stdscr.addstr(0,15,"PUT --> Press <esc> to save changes.")
        stdscr.addstr(2,0,f"{text_buffer}_")
        stdscr.refresh() 

    curses.endwin()

if __name__ == "__main__":
    curses.wrapper(main)
