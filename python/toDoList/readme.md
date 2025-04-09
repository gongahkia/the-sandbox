# To-do

![](https://media.tenor.com/M-ibWYQzmiIAAAAM/cat-cute.gif)

This was the first thing I wanted to build for my own use after learning Python, and while there are quite a few areas where code could be refactored to be much cleaner, I'm proud of this as my first real project. It's functional as a to-do list system in the CLI, although similar to my endeavours in the password manager, permenant saves are written over when the program is reopened. Personally, I really enjoy the fake *loading dialog* in the CLI, gives a very retro vibe.

----------

## Things that could be improved:

1. Need to implement a system that checks for whether a local `.json` file storing past to-dos exists, and if so, should **remove** and **append** all to-dos from that `.json` file, to prevent the aforementioned issue of completely writing over a previous save.
2. Bad user experience seems to be a reuccuring problem I find myself constantly running into. The signposting and instructions could afford to be much clearer overall. 
3. The system for removing and checking items off the to-do list *(with regard to numbering)* is overly cumbersome, and can be greatly simplified in a number of ways. 

----------

This is one of those projects that I definitely want to return to, if not in Python, than in Bash or some other language. I learned a lot from this experience though.

-Gong :)
