{-# LANGUAGE TupleSections,ScopedTypeVariables #-}

module Main where

import Data.Array
import Data.Maybe
import Data.NumInstances

type File = Int
type Rank = Int
type Coors = (File,Rank)

data Colour = White | Black
	deriving (Eq,Show)

coloursToMove = cycle [White,Black]

data PieceType = Pawn | Bishop | Knight | Rook | Queen | King
	deriving (Show,Eq)

type Piece = (Colour,PieceType)

type Board = Array Coors (Maybe Piece)

initialBoard = listArray ((1,1),(8,8)) [
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

data Move =
	Move Coors Coors (Maybe PieceType) |
	Take Coors Coors Coors (Maybe PieceType) |
	EnPassant Coors Coors |
	Castle Coors
	deriving (Eq,Show)

doMove board move = board // case move of
	Move from to promotion          -> [ (from,Nothing), (to,piece from promotion) ]
	Take from to take promotion     -> [ (take,Nothing), (from,Nothing), (to,piece from promotion) ]
	EnPassant from@(_,r1) to@(f2,_) -> [ ((f2,r1),Nothing), (from,Nothing), (to,board!from) ]
	Castle rook@(1,r)               -> let king = (5,r) in
		[ (rook,Nothing), (king,Nothing), ((3,r),board!king), ((4,r),board!rook) ]
	Castle rook@(8,r)               -> let king = (5,r) in
		[ (rook,Nothing), (king,Nothing), ((7,r),board!king), ((6,r),board!rook) ]
	where
	piece from promotion = case promotion of
		Nothing       -> board!from
		Just promoted -> Just (my_colour,promoted) where
			Just (my_colour,_) = board!from

moveGenerator position = [ Move from to promotion |
	(from,Just (colour,piecetype)) <- assocs board,
	colour == colour_to_move,
	(to_d,empties_d) <- movetargets colour_to_move piecetype from,
	Just to <- [map (add_coors from) to_d],
	all isNothing $ map (board!) (to : empties),
	promotion <- case (piecetype,to) of
		(Pawn,(_,rank)) | rank==1 || rank==8 -> map Just [Queen,Knight,Rook,Bishop]
		_ -> [Nothing]
	]
	where
	straight@[south,north,east,west] = [(0,-1),(0,1),(1,0),(-1,0)]
	diagonal = [ north+east,north+west,south+east,south+west ]

	movetargets colour_to_move piecetype from = case (colour_to_move,piecetype,from) of
		(White,Pawn,(_,r)) -> (north,[]) : if r==2 then [(north*2,[north])] else []
		(Black,Pawn,(_,r)) -> (south,[]) : if r==7 then [(south*2,[south])] else []
		(_,Knight,_) -> [ (s,[]) | s <- [
			north*2+east,north*2+west,east*2+north,east*2+south,
			south*2+east,south*2+west,west*2+north,west*2+south ] ]
		(_,Bishop,_) -> [ (s*(l,l), [ s*(i,i) | i <- [1..l] ]) | s <- diagonal, l <- [1..7] ]
		(_,Rook,  _) -> [ (s*(l,l), [ s*(i,i) | i <- [1..l] ]) | s <- straight, l <- [1..7] ]
		(_,Queen, _) -> movetargets colour_to_move Bishop from ++ movetargets colour_to_move Rook from

	board = foldl doMove initialBoard position
	last_move = if length position == 0 then Nothing else Just (last position)
	(colour_to_move,_):_ = reverse $ zip coloursToMove $ map Just position ++ [Nothing]

	add_coors (file,rank) (dx,dy) = case ( file + dx, rank + dy ) of
		(x,y) | x `elem` [1..8] && y `elem` [1..8] -> Just (x,y)
		_ -> Nothing

	piece_move_to White Pawn (x0,y0) (x,y) = (x,y+1) : if y0==2 then [(x,y+2)] else []
	piece_move_to Black Pawn (x0,y0) (x,y) = (x,y-1) : if y0==7 then [(x,y-2)] else []

t = moveGenerator initialPosition