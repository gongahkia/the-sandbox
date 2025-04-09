# Password Manager

![](https://i.imgflip.com/3sh3ec.png)

After learning about how ASCII characters converted to integers, I spent an afternoon creating this random password generator and manager. While some features can definitely be improved, I'm happy, at least for now, with where its at. I especially enjoy the system used to write the passwords and usernames to a local `.json` file 

----------

## Things that could be improved:

1. Currently, the user has to type out the entire string to activate a chosen mode. Should just allow for the first letter of each command to be the default input, and signpost this better in the application.
2. Create a method (perhaps in Bash) to check for when the `.json` file already exists, and if so, to append additional passwords to the `.json` file instead of rewriting the entire file.
3. Something about the entire design of the manager feels clunky. While code is decently refactored, the user experience would be improved with a few reworks.

----------

I'll definitely return to this project one of these days. In the meantime however, its functional at best.

-Gong :)
