{-# LANGUAGE TupleSections #-}

module Main where

import Data.Array
import Data.Maybe

data File = A | B | C | D | E | F | G | H
	deriving (Show,Eq,Ord,Enum)
type Rank = Int

type Coors = (File,Rank)

addIx (dx,dy) (file,rank) = (toEnum $ fromEnum file + dx, 

data Colour = White | Black
	deriving (Eq,Show)

nextToMove White = Black
nextToMove Black = White

data PieceType = Pawn | Bishop | Knight | Rook | Queen | King
	deriving (Show,Eq)

type Piece = (Colour,PieceType)

type Board = Array Coors (Maybe Piece)

initialBoard = listArray ((A,1),(H,8)) [
	w Rook ,w Knight,w Bishop,w King ,w Queen,w Bishop,w Knight,w Rook ,
	w Pawn ,w Pawn  ,w Pawn  ,w Pawn ,w Pawn ,w Pawn  ,w Pawn  ,w Pawn ,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	b Pawn ,b Pawn  ,b Pawn  ,b Pawn ,b Pawn ,b Pawn  ,b Pawn  ,b Pawn ,
	b Rook ,b Knight,b Bishop,b King ,b Queen,b Bishop,b Knight,b Rook ]
	where
	w = Just . (White,)
	b = Just . (Black,)

type Position = [Move]
initialPosition = []

data Move = Move Coors Coors (Maybe Coors) (Maybe PieceType)
	deriving (Eq,Show)

doMove board (Move from to take promotion) =
	board // ( taking ++ [ (from,Nothing), (to,piece) ] )
	where
	piece = case promotion of
		Nothing       -> board!from
		Just promoted -> Just (fst (fromJust (board!from)),promoted) where
			Just (my_colour,_) = board!from
	taking = case take of
		Nothing       -> []
		Just take_on  -> [ (take_on,Nothing) ]

moveGenerator position = [ |
	Just (coors,(colour,piecetype)) <- assocs board,
	colour==colour_to_move,
	]
	where
	board = foldl doMove initialBoard position
	colour_to_move = 

t = moveGenerator initialPosition
