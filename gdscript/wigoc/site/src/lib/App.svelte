<script>
  import Piece from './Piece.svelte';

  let pieces = [
    { name: 'white_pawn', ability: 'Moves forward one square' },
    { name: 'black_rook', ability: 'Moves horizontally or vertically' },
    // Add more pieces as needed
  ];

  let selectedPiece = null;

  function handlePieceClick(piece) {
    selectedPiece = piece;
  }
</script>

<main>
  <div class="chessboard">
    {#each pieces as piece (piece.name)}
      <Piece piece={piece} onClick={() => handlePieceClick(piece)} />
    {/each}
  </div>

  {#if selectedPiece}
    <div class="info">
      <h2>Selected Piece</h2>
      <Piece piece={selectedPiece} />
    </div>
  {/if}
</main>

<style>
  .chessboard {
    display: grid;
    grid-template-columns: repeat(8, 1fr);
    grid-template-rows: repeat(8, 1fr);
    gap: 1px;
    background-image: url('/chessboard.png');
    background-size: cover;
    width: 400px;
    height: 400px;
  }

  .info {
    margin-top: 20px;
  }
</style>
