<script>

// ----- REQUIRED IMPORTS -----

// --- BLACK ---

  import blackPrawn from '../asset/piece/black/black_prawn.png';
  import blackRook from '../asset/piece/black/black_rook.png';
  import blackKnight from '../asset/piece/black/black_knight.png';
  import blackBishop from '../asset/piece/black/black_bishop.png';
  import blackKing from '../asset/piece/black/black_king.png';
  import blackCarp from '../asset/piece/black/black_carp.png';

  // FUA to add more later
  // import black from '../asset/piece/black/black_.png'; 

// --- WHITE ---

  import whitePrawn from '../asset/piece/white/white_prawn.png';
  import whiteRook from '../asset/piece/white/white_rook.png';
  import whiteKnight from '../asset/piece/white/white_knight.png';
  import whiteBishop from '../asset/piece/white/white_bishop.png';
  import whiteKing from '../asset/piece/white/white_king.png';
  import whiteCarp from '../asset/piece/white/white_carp.png';

  // FUA to add more later
  // import white from '../asset/piece/white/white_.png';

  export let piece;
  const pieceImages = {

      // black pieces
        'p': blackPrawn,
        'r': blackRook,
        'n': blackKnight,
        'b': blackBishop,
        'k': blackKing,
        'c': blackCarp,

      // white pieces
        'P': whitePrawn,
        'R': whiteRook,
        'N': whiteKnight,
        'B': whiteBishop,
        'K': whiteKing,
        'C': whiteCarp

    };

</script>

<img src={pieceImages[piece]} alt={piece} />

<style>
    img {
        width: 100%;
        height: 100%;
    }
</style>