{-# LANGUAGE TupleSections #-}

module Main where

import Data.Array
import Data.Maybe

data File = A | B | C | D | E | F | G | H
	deriving (Show,Eq,Ord,Ix)
type Rank = Int

type Coors = (File,Rank)

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
	deriving (Show,Eq)

doMove board (Move from to maybe_take maybe_promotion) =
	board // ( maybe_taking ++ [ (from,Nothing), (to,piece) ] )
	where
	piece = case maybe_promotion of
		Nothing       -> board!from
		Just promoted -> Just (fst (fromJust (board!from)),promoted) where
			Just (my_colour,_) = board!from
	maybe_taking = case maybe_take of
		Nothing       -> []
		Just take_on  -> [ (take_on,Nothing) ]

moveGenerator position = ()
	where
	board = foldl doMove 

t = moveGenerator initialPosition
