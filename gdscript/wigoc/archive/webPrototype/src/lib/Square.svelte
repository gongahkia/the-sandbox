<script>

    import { boardState, selectedPiece, whoseTurn } from './Store.js';
    import Piece from './Piece.svelte';
    import { get } from 'svelte/store';

    export let row;
    export let col;
    export let piece;


/*  
* ----- handleSquareClick function overview ------
    * handles movement logic
    * importantly recall that
        * clicking a square can mean 
            * STEP 1. selecting a piece
            * STEP 2. selecting a destination square to move the piece from STEP 1 to
*/

    const handleSquareClick = () => { 
        
        const currentWhoseTurn = get(whoseTurn); // getter function
        const currentBoardState = get(boardState); // setter function

        const [selectedRow, selectedCol] = get(selectedPiece) || [null, null]; // if selectedPiece == null, sets both selectedRow and selectedCol to null, else if selectedPiece != null, extracts the selectedRow and selectedCol values for that selected piece

        const currentPiece = currentBoardState[selectedRow]?.[selectedCol]; // if selectedRow is null, then the ? enables optional chaining and evaluates to undefined, else accesses the selectedCol and the same optional chaining logic runs, evaluating to either null or a specified string value

        if (selectedPiece && currentPiece) { // covers STEP 2 ;;; if a piece is both selected and that piece exists on the board

            currentBoardState[row][col] = currentPiece; // MOVE the selected piece to that new coordinate
            currentBoardState[selectedRow][selectedCol] = ''; // CLEAR the empty position of the selected piece at the old coordinate

            boardState.set(currentBoardState); // setter function that UPDATES the boardState array
            selectedPiece.set(null); // setter function that UPDATES the selectedPiece by deselecting that piece's coordinates after moving 

            // --- advance turn ---

            if (currentWhoseTurn == "white") {
                whoseTurn.set("black"); // setter function
            } else if (currentWhoseTurn == "black") {
                whoseTurn.set("white"); // setter function
            } else {} // this should never run, just for edge guarding

        } else if (piece) { // covers STEP 1;;; if no piece is currently selected but the clicked square contains a piece

            selectedPiece.set([row, col]); // sets the selectedPiece to the specified value which is retreived and destructured above, then advance to step 2

        } else {} // this should never run, just for edge guarding
    };

</script>

<div class="square" on:click={handleSquareClick}>
    {#if piece}
        <Piece {piece} />
    {/if}
</div>

<style>
    .square {
        width: 50px;
        height: 50px;
        display: flex;
        align-items: center;
        justify-content: center;
        background-color: var(--square-color);
        border: 1px solid black;
    }
    :global(.square:nth-child(even)) {
        --square-color: #f0d9b5;
    }
    :global(.square:nth-child(odd)) {
        --square-color: #b58863;
    }
    :global(.square:hover) {
        background-color: yellow;
    }
</style>
