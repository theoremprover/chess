{-# LANGUAGE TupleSections #-}

module Main where

import Data.Array
import Data.Maybe

type File = Char
type Rank = Int

type Coors = (File,Rank)

data Colour = White | Black
	deriving (Eq,Show)

coloursToMove = cycle [White,Black]

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

pieceMoves White Pawn from@(file,rank) = (
	[ Move from () Nothing maybe_prom | maybe_prom <- ],

type Position = [Move]
initialPosition = []

data Move =
	Move Coors Coors (Maybe PieceType) |
	Take Coors Coors (Maybe PieceType) |
	EnPassant Coors Coors |
	Castle Coors Coors
	deriving (Eq,Show)

doMove board move = board // case move of
	Move from to promotion          -> [ (from,Nothing), (to,piece from promotion) ]
	Take from to take promotion     -> [ (take,Nothing), (from,Nothing), (to,piece from promotion) ]
	EnPassant from@(_,r1) to@(f2,_) -> [ ((f2,r1),Nothing), (from,Nothing), (to,board!from) ]
	Castle rook@('a',r)               -> let king = ('e',r) in
		[ (rook,Nothing), (king,Nothing), (('c',r),board!king), (('d',r),board!rook) ]
	Castle rook@('h',r)               -> let king = ('e',r) in
		[ (rook,Nothing), (king,Nothing), (('g',r),board!king), (('f',r),board!rook) ]
	where
	piece from promotion = case promotion of
		Nothing       -> board!from
		Just promoted -> Just (my_colour,promoted) where
			Just (my_colour,_) = board!from

moveGenerator position = [ |
	Just (coors,(colour,piecetype)) <- assocs board,
	colour==colour_to_move,
	Just move_to <- map (add_coors coors) (piece_to colour piecetype),
	
	]
	where
	board = foldl doMove initialBoard position
	(last_move,colour_to_move) = last $ zip position $ coloursToMove
	piece_to White Pawn = 

	add_coors (file,rank) (dx,dy) = case (fromEnum file + dx, rank + dy ) of
		(x,y) | 
		


t = moveGenerator initialPosition
