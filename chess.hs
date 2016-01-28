{-# LANGUAGE TupleSections #-}

module Main where

import Data.Array
import Data.Maybe
import Data.NumInstances

type File = Char
type Rank = Int

type Coors = (File,Rank)
instance Num Coors where
	

data Colour = White | Black
	deriving (Eq,Show)

coloursToMove = cycle [White,Black]

data PieceType = Pawn | Bishop | Knight | Rook | Queen | King
	deriving (Show,Eq)

type Piece = (Colour,PieceType)

type Board = Array Coors (Maybe Piece)

initialBoard = listArray (('a',1),('h',8)) [
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

{-
pieceMoves White Pawn from@(file,rank) = (
	[ Move from () Nothing maybe_prom | maybe_prom <- ],
-}

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
	Castle rook@('a',r)               -> let king = ('e',r) in
		[ (rook,Nothing), (king,Nothing), (('c',r),board!king), (('d',r),board!rook) ]
	Castle rook@('h',r)               -> let king = ('e',r) in
		[ (rook,Nothing), (king,Nothing), (('g',r),board!king), (('f',r),board!rook) ]
	where
	piece from promotion = case promotion of
		Nothing       -> board!from
		Just promoted -> Just (my_colour,promoted) where
			Just (my_colour,_) = board!from

moveGenerator position = [ Move from to promotion |
	(from,Just (colour,piecetype)) <- assocs board,
	colour == colour_to_move,
	(to,empties) <- movetargets colour_to_move piecetype from
	all isNothing $ map (board!) (to:empties),
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
		(_,Bishop,_) -> [ (s*l,empties) | s <- diagonal, l <- [1..7], empties <- [ s*i | i <- [1..l] ] ]
		(_,Rook,_)   -> [ (s*l,empties) | s <- straight, l <- [1..7], empties <- [ s*i | i <- [1..l] ] ]
		(_,Queen,_) -> movetargets colour_to_move Bishop from ++ movetargets colour_to_move Rook from

	board = foldl doMove initialBoard position
	(colour_to_move,_):(_,last_move):_ = reverse $ zip moveColours $ map Just position ++ [Nothing]

	add_coors (file,rank) (dx,dy) = case ( fromEnum file + dx, rank + dy ) of
		(x,y) | x `elem` ['a'..'h'] && y `elem` [1..8] -> Just (toEnum x,y)
		_ -> Nothing
	
	piece_move_to White Pawn (x0,y0) (x,y) = (x,y+1) : if y0==2 then [(x,y+2)] else []
	piece_move_to Black Pawn (x0,y0) (x,y) = (x,y-1) : if y0==7 then [(x,y-2)] else []

t = moveGenerator initialPosition