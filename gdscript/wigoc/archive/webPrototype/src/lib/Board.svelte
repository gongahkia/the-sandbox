<script>

    import { boardState } from './Store.js';
    import Square from './Square.svelte';
    import { get } from 'svelte/store';

    let squares = get(boardState); // getter function to retrieve the local storage value boardState

    boardState.subscribe(value => { // subscribe to watch for changes to value of boardState
        squares = value;
    });

    let boardSize = 8; // can dynamically define a different boardSize later!!!

</script>

<div class="board">
    {#each { length: boardSize } as _, row}
        <div class="row">
            {#each { length: boardSize } as _, col}
                <Square {row} {col} piece={squares[row][col]} />
            {/each}
        </div>
    {/each}
</div>

<style>
    .board {
        display: grid;
        grid-template-columns: repeat(8, 1fr);
        width: 400px;
        height: 400px;
    }
    .row {
        display: contents;
    }
</style>
