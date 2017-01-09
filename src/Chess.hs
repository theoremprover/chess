{-# LANGUAGE UnicodeSyntax,RecordWildCards #-}

module Main where

import Data.Array
import Data.Maybe
import Data.Ix
import Data.Char
import Data.List
import Data.Tuple
import Data.NumInstances

infixl 6 +@

data Piece = Ù | Ú | Û | Ü | Ý | Þ deriving (Show,Eq,Enum)
data Colour = White | Black deriving (Show,Eq,Enum)
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

Nothing +@ _ = Nothing
(Just (file,rank)) +@ (δfile,δrank) = 

moveGen Position{..} = concatMap piece_moves (assocs pBoard) where
	piece_moves (from,Just (col,piece)) | col == pColourToMove = case piece of
		Ú -> targets from
				[ north+2*east,north+2*west,2*north+west,2*north+east,south+2*west,south+2*east,2*south+west,2*south+east ]
				
		Ù -> []
		_ -> [] {-
			Û -> []
			Ü -> []
			Ý -> []
			Þ -> []
-}
	piece_moves _ = []

	(north,south,east,west) = ((0,1),(0,-1),(1,0),(-1,0))
	-- ∂
	add_dir (file,rank) (dfile,drank) = case (fromEnum file + dfile,fromEnum rank + drank) of
		(ifile,irank) | ifile ´elem´ [0..7] && irank `elem` [0..7] -> 
	targets from directions = []
