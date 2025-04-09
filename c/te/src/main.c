// FUA
    // immediate
        // basic user input keypresses
        // basic text rendering
        // writing and reading from files
        // implement vim binds
        // write my own header files
        // minimal syntax highlighting for detected markdown
        // otherwise just a pure text editor
        // allow further configuration as necessary
            // user-defined themes
            // auto-complete brackets
    // references
        // follow up from here https://www.sbarjatiya.com/notes_wiki/index.php/Using_ncurses_library_with_C
        // further inspiration => https://viewsourcecode.org/snaptoken/kilo/

#include <ncurses.h>
#include <string.h>

int main(){	

    // ----- curses defaults -----

    initscr(); // initialises curses terminal
    cbreak(); // reads every inputted character as its keycode without waiting for CR (carriage return) to be pressed
    keypad(stdscr, TRUE); // enables other character keycodes to be read
    noecho(); // switch off character echoing in C

    // ----- variable instantiation -----

    int row, col;
    char title[] = "Ukiyo\n";
    char description[] = "the bare-bones text editor\n";
    char instruction[] = "Enter text here:\n";

    // size of the window
    getmaxyx(stdscr,row,col);
    mvprintw(row/2,(col-strlen(title))/2,"%s",title);
    mvprintw(row/2 + 1,(col-strlen(description))/2,"%s",description);
    mvprintw(row-2,0,instruction,row,col);
    // mvprintw(row-2,0,"the screen has %d rows and %d columns\n",row,col); -- print debug information to the console
    
    char chr = getch();
    refresh();

    if (chr == KEY_F(2)){
        printw("f2 pressed");
    } else {
        printw("pressed key is ");
        attron(A_ITALIC);
        attron(A_BOLD);
        printw("%c", chr);
        attroff(A_ITALIC);
        attroff(A_BOLD);
    }

	refresh(); // refreshes terminal
	getch(); // accepts user inpuIt
	endwin(); // ends curses and closes terminal

	return 0;
}
