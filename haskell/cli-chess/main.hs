import System.Random
import Data.Char
import Data.List
import Control.Monad

data Piece = Piece { pieceType :: PieceType, pieceColor :: Color } deriving (Eq)
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq)
data Color = White | Black deriving (Eq)

type Board = [[Maybe Piece]]
type Position = (Int, Int)

initialBoard :: Board
initialBoard = [
    [r, n, b, q, k, b, n, r],
    [p, p, p, p, p, p, p, p],
    replicate 8 Nothing,
    replicate 8 Nothing,
    replicate 8 Nothing,
    replicate 8 Nothing,
    map (fmap $ \p -> p { pieceColor = White }) [p, p, p, p, p, p, p, p],
    map (fmap $ \p -> p { pieceColor = White }) [r, n, b, q, k, b, n, r]
  ]
  where
    r = Just $ Piece Rook Black
    n = Just $ Piece Knight Black
    b = Just $ Piece Bishop Black
    q = Just $ Piece Queen Black
    k = Just $ Piece King Black
    p = Just $ Piece Pawn Black

pieceToUnicode :: Piece -> Char
pieceToUnicode (Piece pt c) = case (pt, c) of
    (King,   White) -> '♔'
    (Queen,  White) -> '♕'
    (Rook,   White) -> '♖'
    (Bishop, White) -> '♗'
    (Knight, White) -> '♘'
    (Pawn,   White) -> '♙'
    (King,   Black) -> '♚'
    (Queen,  Black) -> '♛'
    (Rook,   Black) -> '♜'
    (Bishop, Black) -> '♝'
    (Knight, Black) -> '♞'
    (Pawn,   Black) -> '♟'

drawBoard :: Board -> String
drawBoard board = unlines $ 
    [" abcdefgh"] ++
    [show (8-i) ++ "|" ++ concatMap drawSquare row ++ "|" ++ show (8-i) | (i, row) <- zip [0..] board] ++
    [" abcdefgh"]
  where
    drawSquare Nothing = "."
    drawSquare (Just piece) = [pieceToUnicode piece]

main :: IO ()
main = do
    putStrLn "Welcome to CLI Chess!"
    gameLoop initialBoard White

gameLoop :: Board -> Color -> IO ()
gameLoop board turn = do
    putStrLn $ drawBoard board
    if turn == White
        then do
            putStrLn "Your move (e.g., e2e4):"
            move <- getLine
            case makeMove board move White of
                Just newBoard -> gameLoop newBoard Black
                Nothing -> do
                    putStrLn "Invalid move. Try again."
                    gameLoop board White
        else do
            putStrLn "Computer's move:"
            newBoard <- computerMove board
            gameLoop newBoard White

makeMove :: Board -> String -> Color -> Maybe Board
makeMove board [fromFile, fromRank, toFile, toRank] color
    | isValidMove board from to color = Just $ movePiece board from to
    | otherwise = Nothing
  where
    from = (ord fromFile - ord 'a', 8 - (ord fromRank - ord '0'))
    to = (ord toFile - ord 'a', 8 - (ord toRank - ord '0'))
makeMove _ _ _ = Nothing

isValidMove :: Board -> Position -> Position -> Color -> Bool
isValidMove board (fx, fy) (tx, ty) color =
    inBounds (fx, fy) && inBounds (tx, ty) &&
    case board !! fy !! fx of
        Just (Piece _ c) -> c == color
        _ -> False
  where
    inBounds (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

movePiece :: Board -> Position -> Position -> Board
movePiece board (fx, fy) (tx, ty) =
    [[if (x,y) == (tx,ty) then board !! fy !! fx
      else if (x,y) == (fx,fy) then Nothing
      else board !! y !! x | x <- [0..7]] | y <- [0..7]]

computerMove :: Board -> IO Board
computerMove board = do
    let blackPieces = [(x,y) | x <- [0..7], y <- [0..7], 
                       case board !! y !! x of
                           Just (Piece _ Black) -> True
                           _ -> False]
    (fx, fy) <- randomChoice blackPieces
    let possibleMoves = [(tx,ty) | tx <- [0..7], ty <- [0..7], 
                         isValidMove board (fx,fy) (tx,ty) Black]
    (tx, ty) <- randomChoice possibleMoves
    return $ movePiece board (fx,fy) (tx,ty)

randomChoice :: [a] -> IO a
randomChoice xs = do
    i <- randomRIO (0, length xs - 1)
    return $ xs !! i