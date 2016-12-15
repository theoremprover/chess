{-# LANGUAGE UnicodeSyntax,RecordWildCards #-}

module Main where

import Data.Array
import Data.Maybe
import Data.Ix
import Data.Char
import Data.List

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
		[ "¿" ++ replicate 8 'À' ++ "Á" ] ++
		map show_rank [Eighth,Seventh .. First] ++
		[ "Ä" ++ map (toEnum.(+0xce)) [1..8] ++ "Æ" ] ++
		[ show pColourToMove ++ " to move" ]
		where
		show_rank rank = [ toEnum $ 0xc7 + index (First,Eighth) rank ] ++
			map (show_square rank) [A .. H] ++ "Ã"
		show_square rank file = toEnum $ 0xd8 + mod (index (First,Eighth) rank + index (A,H) file) 2 * 15 +
			case pBoard!(file,rank) of
				Nothing             -> 0
				Just (colour,piece) -> 1 + fromEnum colour * 6 + fromEnum piece
