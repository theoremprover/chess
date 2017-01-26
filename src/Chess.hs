{-# LANGUAGE UnicodeSyntax,RecordWildCards,TypeSynonymInstances,FlexibleInstances,OverlappingInstances #-}

module Main where

import Data.Array
import Data.Maybe
import Data.Ix
import Data.Char
import Data.List
import Data.Tuple
import Data.NumInstances

data Piece = Ù | Ú | Û | Ü | Ý | Þ deriving (Show,Eq,Enum)
data Colour = White | Black deriving (Show,Eq,Enum)
nextColour White = Black
nextColour Black = White
data File = A | B | C | D | E | F | G | H
	deriving (Show,Ix,Ord,Eq,Enum)
data Rank = First | Second | Third | Fourth | Fifth | Sixth | Seventh | Eighth
	deriving (Ix,Ord,Eq,Enum)
instance Show Rank where
	show rank = show $ index (First,Eighth) rank + 1
type Coors = (File,Rank)
type Board = Array Coors (Maybe (Colour,Piece))
data Position = Position {
	pBoard              :: Board,
	pColourToMove       :: Colour,
	pCanCastleQueenSide :: [Colour],
	pCanCastleKingSide  :: [Colour],
	pEnPassantSquare    :: Maybe Coors,
	pHalfmoveClock      :: Int,
	pMoveCounter        :: Int }
	deriving (Eq)

initialPosition = Position {
	pBoard = boardFromString [
		"âïáòäðàñ",
		"îßîßîßîß",
		"ØçØçØçØç",
		"çØçØçØçØ",
		"ØçØçØçØç",
		"çØçØçØçØ",
		"ÙèÙèÙèÙè",
		"ëÚêÝíÛéÜ" ],
	pColourToMove       = White,
	pCanCastleQueenSide = [White,Black],
	pCanCastleKingSide  = [White,Black],
	pEnPassantSquare    = Nothing,
	pHalfmoveClock      = 0,
	pMoveCounter        = 0 }

boardFromString s = listArray ((A,First),(H,Eighth)) $ map to_fig $ concat $ map reverse $ transpose s
	where
	to_fig c_rep = case if c_rep < 'ç' then c_rep else chr $ ord c_rep - ord 'ç' + ord 'Ø' of
		c | c `elem` "ÙÚÛÜÝÞ" -> Just (White,toEnum (ord c - ord 'Ù'))
		c | c `elem` "ßàáâãä" -> Just (Black,toEnum (ord c - ord 'ß'))
		_ -> Nothing

instance Show Position where
	show Position{..} = unlines $
		[ "¿ÀÀÀÀÀÀÀÀÁ" ] ++
		map show_rank [Eighth,Seventh .. First] ++
		[ "ÄÏÐÑÒÓÔÕÖÆ" ] ++
		[ show pColourToMove ++ " to move" ]
		where
		show_rank rank = [ "ÇÈÉÊËÌÍÎ" !! fromEnum rank ] ++
			map (show_square rank) [A .. H] ++ "Ã"
		show_square rank file = case pBoard!(file,rank) of
				Nothing -> if darksquare then 'ç' else 'Ø'
				Just (White,piece) | darksquare -> "èéêëìí" !! (fromEnum piece)
				Just (White,piece)              -> "ÙÚÛÜÝÞ" !! (fromEnum piece)
				Just (Black,piece) | darksquare -> "îïðñòó" !! (fromEnum piece)
				Just (Black,piece)              -> "ßàáâãä" !! (fromEnum piece)
			where
			darksquare = mod (fromEnum rank + fromEnum file) 2 == 0

infixl 6 +@
(+@) :: Maybe Coors -> (Int,Int) -> Maybe Coors
(Just (file,rank)) +@ (δfile,δrank) | ifile `elem` [0..7] && irank `elem` [0..7] =
	Just (toEnum ifile,toEnum irank)
	where
	(ifile,irank) = (fromEnum file + δfile,fromEnum rank + δrank)
_ +@ _ = Nothing

instance Show Coors where
	show (file,rank) = show file ++ show (fromEnum rank + 1)

data Move = Move {
	moveFrom :: Coors, moveTo :: Coors, moveTakes :: Maybe Coors, movePromote :: Maybe Piece }
	deriving Eq
instance Show Move where
	show Move{..} = show moveFrom ++ if isJust moveTakes then "x" else "" ++ show moveTo ++ case movePromote of
		Nothing -> ""
		Just piece -> show piece

moveGen Position{..} = concatMap piece_moves (assocs pBoard) where

	(north,south,east,west) = ((0,1),(0,-1),(1,0),(-1,0))

	piece_moves (from,Just (col,piece)) | col == pColourToMove = case piece of
		Ú -> concatMap (try_move_to from) [ north+2*east,north+2*west,2*north+west,2*north+east,south+2*west,south+2*east,2*south+west,2*south+east ]
		Û -> concatMap (rec_move 1 from) [ north+east,north+west,south+east,south+west ]
		_ -> []
	piece_moves _ = []

	rec_move :: Int -> Coors -> (Int,Int) -> [Move]
	rec_move i from (f,r) = case try_move_to from (f*i,r*i) of
		moves@(Move _ to Nothing _ : _) -> moves ++ rec_move (i+1) from (f,r)
		moves -> moves

	try_move_to :: Coors -> (Int,Int) -> [Move]
	try_move_to from δ = case (Just from) +@ δ of
		Nothing -> []
		Just to@(_,rank) -> case pBoard!to of
			Nothing                             -> [ Move from to Nothing promo   | promo <- promotions ]
			Just (col,_) | col /= pColourToMove -> [ Move from to (Just to) promo | promo <- promotions ]
			_ -> []
			where
			promotions = case pBoard!from of
				Just (White,Ù) | rank==Seventh -> map Just [Ú,Û,Ü,Ý]
				Just (Black,Ù) | rank==Second  -> map Just [Ú,Û,Ü,Ý]
				_ -> [ Nothing ]

doMove Move{..} Position{..} = Position {
	pBoard				= pBoard // ( move moveFrom moveTo ++ mb_take ++ mb_castling ),
	pColourToMove       = nextColour pColourToMove,
	pCanCastleQueenSide = (if no_castling_queenside_any_more then delete pColourToMove else id) pCanCastleQueenSide,
	pCanCastleKingSide  = (if no_castling_kingside_any_more  then delete pColourToMove else id) pCanCastleKingSide,
	pEnPassantSquare    = Nothing,
	pHalfmoveClock      = pHalfmoveClock + 1,
	pMoveCounter        = pMoveCounter + 1
	}
	where

	move from to = [ (from,Nothing), (to,pBoard!from) ]

	mb_take = case moveTakes of
		Nothing    -> []
		Just coors -> [ (coors,Nothing) ]

	Just (_,moved_piece) = pBoard!moveFrom

	mb_castling = case (moved_piece,moveFrom,moveTo) of
		(Þ,(E,rank),(G,_)) -> move (H,rank) (F,rank)
		(Þ,(E,rank),(C,_)) -> move (A,rank) (D,rank)
		_                   -> []

	(no_castling_queenside_any_more,no_castling_kingside_any_more) = case (moveFrom,pColourToMove) of
		_ | moved_piece==Þ -> (True,True)
		((H,First),White)  -> (False,True)
		((A,First),White)  -> (True,False)
		((H,Eighth),Black) -> (False,True)
		((A,Eighth),Black) -> (True,False)
		_                  -> (False,False)

{-
		Ù -> []
		_ -> []
			Û -> []
			Ü -> []
			Ý -> []
			Þ -> []

	-- ∂
	add_dir (file,rank) (dfile,drank) = 
	try_move_to from directions = []
-}
