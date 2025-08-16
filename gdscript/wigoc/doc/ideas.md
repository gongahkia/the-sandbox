> [!NOTE]
> FOLLOW UP ACTIONS
> 
> 1. Get the base normal chessboard rendering in Godot with 2 player support
> 2. Decide cost of each piece
> 3. Add basic wiki page for WIGOC render the frontend in Vanilla HTML or Svelte as some frontend
> 4. Finish drawings for all pieces
> 5. If Godot can invert the color of a PNG, then tweak `./test/piece.py` and remove the differentiation between black and white pieces

# Ideas

## Product

* Godot game
    * Use Godot3 or Godot4
    * comprises the following
        1. 2-player WIGOC board 
            * board *(consider making the board flip, assuming 2 players are playing from different ends of the SAME phone)*
            * graveyard to the left *or* right side of the board
        2. UI for each player's side to show 
            * counters for pieces with recharging or fixed number of use abilities
    * Distribute via itch.io
* Web frontend
    * comprises the following 
        1. Wiki
            * introduces the pieces
            * introduces how to play WIGOC *(gamestate eg. piece drafting in buying phase, gameplay phase)*
        2. Link to download WIGOC as a Godot game
    * in Svelte or Vanilla JS, HTML and CSS
