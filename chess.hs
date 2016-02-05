{-# LANGUAGE TupleSections,ScopedTypeVariables #-}

module Main where

import Data.Array
import Data.Maybe
import Data.NumInstances
import Control.Monad

type File = Int
type Rank = Int
type Coors = (File,Rank)

data Colour = White | Black
	deriving (Eq,Show,Enum)

coloursToMove = cycle [White,Black]

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
	deriving (Show,Eq,Enum)

type Piece = (Colour,PieceType)

type Board = Array Coors (Maybe Piece)

initialBoard = array ((1,1),(8,8)) $ zip [ (f,r) | r <- [8,7..1], f <- [1..8] ] [
	b Rook ,b Knight,b Bishop,b Queen,b King, b Bishop,b Knight,b Rook ,
	b Pawn ,b Pawn  ,b Pawn  ,b Pawn ,b Pawn ,b Pawn  ,b Pawn  ,b Pawn ,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	w Pawn ,w Pawn  ,w Pawn  ,w Pawn ,w Pawn ,w Pawn  ,w Pawn  ,w Pawn ,
	w Rook ,w Knight,w Bishop,w Queen,w King, w Bishop,w Knight,w Rook ]
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

createBoard position = foldl doMove initialBoard position

moveGenerator position = [ move |
	(from,Just (colour,piecetype)) <- assocs board,
	colour == colour_to_move,
	(to_rel,mb_take,empties_d) <- case (colour,piecetype,from) of
		(White,Pawn,(_,r)) -> (north,Nothing,[]) : if r==2 then [(north*2,Nothing,[north])] else []
		(Black,Pawn,(_,r)) -> (south,Nothing,[]) : if r==7 then [(south*2,Nothing,[south])] else []
		


	(move,empties_d) <- [ Move from to promotion | 
		(to_d,empties_d) <- movetargets colour_to_move piecetype from,
		Just to <- [addrelcoors from to_d],
		isNothing $ board!to,
	all isNothing $ map (board!) $ catMaybes $ map (addrelcoors from) empties_d,
	promotion <- case (piecetype,to) of
		(Pawn,(_,rank)) | rank==1 || rank==8 -> map Just [Queen,Knight,Rook,Bishop]
		_ -> [Nothing] ]
	where
	straight@[south,north,east,west] = [(0,-1),(0,1),(1,0),(-1,0)]
	diagonal = [ north+east,north+west,south+east,south+west ]
	addrelcoors (file,rank) (dx,dy) = case ( file + dx, rank + dy ) of
		(x,y) | x `elem` [1..8] && y `elem` [1..8] -> Just (x,y)
		_ -> Nothing

	movetargets colour_to_move piecetype from = case (colour_to_move,piecetype,from) of
		(White,Pawn,(_,r)) -> (north,[]) : if r==2 then [(north*2,[north])] else []
		(Black,Pawn,(_,r)) -> (south,[]) : if r==7 then [(south*2,[south])] else []
		(_,Knight,_) -> [ (s,[]) | s <- [
			north*2+east,north*2+west,east*2+north,east*2+south,
			south*2+east,south*2+west,west*2+north,west*2+south ] ]
		(_,Bishop,_) -> [ (s*(l,l), [ s*(i,i) | i <- [1..l] ]) | s <- diagonal, l <- [1..7] ]
		(_,Rook,  _) -> [ (s*(l,l), [ s*(i,i) | i <- [1..l] ]) | s <- straight, l <- [1..7] ]
		(_,Queen, _) -> movetargets colour_to_move Bishop from ++ movetargets colour_to_move Rook from
		(_,King,  _) -> map (,[]) $ diagonal++straight

	taketargets colour_to_move piecetype from = case (colour_to_move,piecetype) of
		(White,Pawn) -> [ (north+east,[]), (north+west,[]) ]
		(Black,Pawn) -> [ (south+east,[]), (south+west,[]) ]
		_ -> movetargets colour_to_move piecetype from

	eptargets colour_to_move piecetype from = case (colour_to_move,piecetype,from,reverse position) of
		(White,Pawn,(file,2),Move : _) -> [ (north+east,[]), (north+west,[]) ]
		(Black,Pawn,(file,7)) -> [ (south+east,[]), (south+west,[]) ]
		_ -> []
		where
		last_move = last position

	board = createBoard position
	last_move = if length position == 0 then Nothing else Just (last position)
	(colour_to_move,_):_ = reverse $ zip coloursToMove $ map Just position ++ [Nothing]

putStrConsoleLn s = do
	putStrLn s
	appendFile "test.txt" (s++"\n")

showPos position = do
	let board = createBoard position
	putStrConsoleLn $ "\xbf" ++ replicate 8 '\xc0' ++ "\xc1"
	forM_ [8,7..1] $ \ rank -> do
		line <- forM [1..8] $ \ file -> do
			return $ toEnum $ 0xd7 + mod (file+rank) 2 * 15 + case board!(file,rank) of
				Nothing             -> 0
				Just (colour,piece) -> 1 + fromEnum colour * 6 + fromEnum piece
		putStrConsoleLn $ [toEnum $ 0xc6 + rank ] ++ line ++ "\xc3"
	putStrConsoleLn $ "\xc4" ++ map (toEnum.(+0xce)) [1..8] ++ "\xc6"

main = do
	writeFile "test.txt" ""
	forM_ (moveGenerator initialPosition) $ \ move -> do
		putStrConsoleLn "=================================="
		putStrConsoleLn $ show move
		showPos [move]
		s <- getLine
		return ()