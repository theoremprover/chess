We will use unicode symbols in the code and some standard set of compiler extensions...

> {-# LANGUAGE UnicodeSyntax,RecordWildCards,TypeSynonymInstances,FlexibleInstances,OverlappingInstances,TupleSections #-}

> module Main where
> 
> import Data.Set (Set)
> import qualified Data.Set as Set
> import Data.Char
> import Data.Array

In chess, two players

> data Colour = White | Black
> 	deriving (Show,Eq,Enum,Bounded,Ord)

are taking turns

> nextColour White = Black
> nextColour Black = White

in moving pieces

> data Piece = Ù | Ú | Û | Ü | Ý | Þ
> 	deriving (Eq,Enum,Bounded,Ord,Show)

on a board, which is an array of squares indexed by coordinates:

> type Board = Array Coors Square
> type Square = Maybe (Colour,Piece)

On a square, maybe there is a piece of a colour, or nothing.

In chess, the file coordinate is a letter

> data File = A | B | C | D | E | F | G | H
>	deriving (Show,Eq,Ix,Ord,Enum)

and the rank is an integer number.
Hence, the coordinates are the (cartesian) product

> type Coors = (File,Int)
> instance Show Coors where
>	show (file,rank) = map toLower (show file) ++ show rank


A position in a chess game consists of
the current board,
the colour to move,
the set of players that still have the right to castle queen side or king side,
whether a pawn could be taken en passant in the next move,
and a clock counting the half moves that have been made
(chess rules say that the game is drawn if for 50+ half moves, no pawn is moved or piece is taken).

> data Position = Position {
> 	pBoard              :: Board,
> 	pColourToMove       :: Colour,
> 	pCanCastleQueenSide :: Set Colour,
> 	pCanCastleKingSide  :: Set Colour,
> 	pEnPassantSquare    :: Maybe Coors,
> 	pHalfmoveClock      :: Int,
>	pNextMoveNumber     :: Int }
>-- 	deriving Eq

In the initial position, White is to start,
with both players having all castling rights.
There is no pawn that could be taken en passant,
and the half move clock starts at zero:

> initialPosition = Position {
> 	pBoard = boardFromString $
> 		"âïáòäðàñ" ++
> 		"îßîßîßîß" ++
> 		"ØçØçØçØç" ++
> 		"çØçØçØçØ" ++
> 		"ØçØçØçØç" ++
> 		"çØçØçØçØ" ++
> 		"ÙèÙèÙèÙè" ++
> 		"ëÚêÝíÛéÜ",
> 	pColourToMove       = White,
> 	pCanCastleQueenSide = Set.fromList allOfThem,
> 	pCanCastleKingSide  = Set.fromList allOfThem,
> 	pEnPassantSquare    = Nothing,
> 	pHalfmoveClock      = 0,
>	pNextMoveNumber     = 1 }

> allOfThem :: (Enum a,Bounded a,Ord a) => [a]
> allOfThem = enumFromTo minBound maxBound

The allOfThem function generates a list of all values of the respective type.
In order to print a board, we define a show_square function:

> show_square darksquare square = case square of 
> 	Nothing            | darksquare -> 'ç'
> 	Nothing                         -> 'Ø'
> 	Just (White,piece) | darksquare -> "èéêëìí" !! (fromEnum piece)
> 	Just (White,piece)              -> "ÙÚÛÜÝÞ" !! (fromEnum piece)
> 	Just (Black,piece) | darksquare -> "îïðñòó" !! (fromEnum piece)
> 	Just (Black,piece)              -> "ßàáâãä" !! (fromEnum piece)

read_square is the inverse of show_square, so we don't have to give both directions
and thus have single source.

> read_square :: Char -> Square
> read_square c = lookup c [ (show_square dark (Just (col,piece)), (col,piece)) |
>	col <- allOfThem, piece <- allOfThem, dark <- allOfThem ]

> boardFromString s = array ((A,1),(H,8)) $ zip [ (f,r) | r <- [8,7..1], f <- [A .. H] ] (map read_square s)

The boardFromString convenience function converts a string to
a board with pieces on it.
The resulting board is an array with an index ranging from (A,1) to (H,8),
starting from (A,8) in the upper left corner.

In order to print a chess position, we make Position an instance of Show:

> instance Show Position where
> 	show Position{..} =
> 		"¿ÀÀÀÀÀÀÀÀÁ\n" ++
> 		unlines (map show_rank [8,7..1]) ++
> 		"ÄÏÐÑÒÓÔÕÖÆ\n" ++
> 		show pColourToMove ++ " to move\n"
> 		where
> 		show_rank rank = [ "ÇÈÉÊËÌÍÎ" !! (fromEnum rank - 1) ] ++
>			[ show_square (is_darksquare (file,rank)) (pBoard!(file,rank)) | file <- [A .. H] ] ++ "Ã"
> 			where
> 			is_darksquare (file,rank) = mod (fromEnum rank + fromEnum file) 2 == 1

In order to calculate target square coordinates, we need to add indices to coordinates.
Therefore we define a new infix operator, which is left associative and has precedence 6
(same as the normal "+" operator).
The target coordinates may be out of the board's bounds, which is indicated by "Nothing".

> infixl 6 +++
> (+++) :: Maybe Coors -> (Int,Int) -> Maybe Coors
> Nothing          +++ _             = Nothing
> Just (file,rank) +++ (δfile,δrank) | ifile' `elem` [0..7] && rank' `elem` [1..8]
> 									 = Just (toEnum ifile',rank')
>	where
> 	(ifile',rank') = (fromEnum file + δfile, rank + δrank)

A normal move goes from a coordinate to another coordinate, might take an opponent's piece,
and might also promote a pawn to another piece.

> data CastlingSide = Queenside | Kingside
> data Move =
>	Move {
> 		moveFrom    :: Coors,
> 		moveTo      :: Coors,
> 		moveTakes   :: Maybe Coors,
> 		movePromote :: Maybe Piece } |
>	Castling CastlingSide
>		
> instance Show Move where
> 	show Move{..} = show moveFrom ++ maybe "" (const "x") moveTakes ++
>		show moveTo ++ case movePromote of
> 			Nothing -> ""
> 			Just Ú -> "N"
> 			Just Û -> "B"
> 			Just Ü -> "R"
> 			Just Ý -> "Q"
>	show (Castling Queenside) = "O-O-O"
>	show (Castling Kingside)  = "O-O"

> doMove pos@Position{..} move = pos' {
> 	pCanCastleQueenSide = if disabled_queenside then pCanCastleQueenSide else Set.delete pColourToMove pCanCastleQueenSide,
> 	pCanCastleKingSide  = if disabled_kingside  then pCanCastleKingSide  else Set.delete pColourToMove pCanCastleKingSide,
> 	pColourToMove  = nextColour pColourToMove,
>	pHalfmoveClock = case move of
>		Move _    _ (Just _) _ -> 0
>		Move from _ _ _ | pBoard!from == Just (pColourToMove,Ù) -> 0
>		_ -> pHalfmoveClock + 1,
> 	pNextMoveNumber = pNextMoveNumber + 1 }
> 	where
>	moved_piece = case move of
>		Castling _      -> Þ
>		Move from _ _ _ -> fromJust $ pBoard!from
>	(disabled_queenside,disabled_kingside) = case (pColourToMove,move) of
> 		(_,Castling _)           -> (False,False)
> 		(White,Move (E,1) _ _ _) -> (False,False)
> 		(Black,Move (E,8) _ _ _) -> (False,False)
> 		(White,Move (A,1) _ _ _) -> (False,True )
> 		(Black,Move (A,8) _ _ _) -> (True ,False)
> 		(White,Move (H,1) _ _ _) -> (False,True )
> 		(Black,Move (H,8) _ _ _) -> (True ,False)
>		_					     -> (True ,True )
> 	pos' = pos { pBoard = pBoard // case move of
> 		Move{..} -> maybe [] (\ take_coors -> [(take_coors,Nothing)]) moveTakes ++
>			[ (moveFrom,Nothing), (moveTo,maybe (pBoard!moveFrom) Just.(pColourToMove,) movePromote) ]
> 		Castling Queenside -> [ ((A,r),Nothing), ((D,r),pBoard!(A,r)), ((E,r),Nothing), ((C,r),pBoard!(E,r)) ]
> 		Castling Kingside  -> [ ((H,r),Nothing), ((F,r),pBoard!(H,r)), ((E,r),Nothing), ((G,r),pBoard!(E,r)) ] } where
>		(r,_) = baseAndPawnRank pColourToMove

The definition for the base and pawn start rank for each colour is needed above (and even more often):

> baseAndPawnRank White = (1,2)
> baseAndPawnRank Black = (8,7)
