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

nextColour White = Black
nextColour Black = White
coloursToMove = iterate nextColour White

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

data Move = Move Coors Coors (Maybe Coors) (Maybe PieceType)
	deriving (Eq,Show)

doMove board (Move from to mb_take mb_promotion) = board // (
	maybe [] ((:[]).(,Nothing)) mb_take ++
	(from,Nothing) :
	(to,case mb_promotion of
		Nothing       -> board!from
		Just promoted -> Just (my_colour,promoted) where
			Just (my_colour,_) = board!from ) :
	case (from,to,board!from) of
		((5,r),(7,_),(Just (colour,King))) -> [ ((8,r),Nothing),((6,r),Just (colour,Rook)) ]
		((5,r),(3,_),(Just (colour,King))) -> [ ((1,r),Nothing),((4,r),Just (colour,Rook)) ]
		_ -> [] )

moveGenerator moves board = [ Move from to mb_take mb_promotion |
	(from,Just (colour,piecetype)) <- assocs board,
	colour == colour_to_move,
	(to_rel,mb_take_rel,empties_d) <- let

		(pawn_dir,pawn_initial_file) = if colour==White then (north,2) else (south,7)

		move_or_take (to_rel,empties) = case addrelcoors from to_rel of
			Nothing -> Nothing
			Just to -> case board!to of
				Nothing -> Just (to_rel,Nothing,to:empties)
				Just (other_colour,_) | other_colour/=colour -> Just (to_rel,Just to_rel,empties)
				_ -> Nothing

		can_take_rel on_rel = case addrelcoors from on_rel of
			Nothing -> False
			Just on -> can_take colour on

		in case (piecetype,from) of
			(Pawn,(f,r)) -> (pawn_dir,Nothing,[pawn_dir]) :
				(if r == pawn_initial_file then [(pawn_dir*2,Nothing,[pawn_dir,pawn_dir*2])] else []) ++
				[ (pawn_dir+hor,Just (pawn_dir+hor),[]) | hor <- [east,west], can_take_rel (pawn_dir+hor) ] ++
				[ (pawn_dir+hor,Just hor,[]) | hor <- [east,west], can_take_rel hor,
					((Move last_from last_to Nothing Nothing):_) <- [reverse moves],
					Just (last_colour,Pawn) <- [board!last_to], nextColour last_colour == colour,
					last_from == from+2*pawn_dir+hor, last_to == from+hor ]
			_ -> catMaybes $ map move_or_take (move_targets piecetype),

	Just to <- [ addrelcoors from to_rel ],
	mb_take <- case mb_take_rel of
		Nothing -> [Nothing]
		Just take_rel -> case addrelcoors from take_rel of
			Nothing -> []
			Just take_on -> if (can_take colour take_on) then [Just take_on] else [],

	all isNothing $ map (board!) $ catMaybes $ map (addrelcoors from) empties_d,
	mb_promotion <- case (piecetype,to) of
		(Pawn,(_,rank)) | rank==1 || rank==8 -> map Just [Queen,Knight,Rook,Bishop]
		_ -> [Nothing]
	]

	where

	can_take mycolour on = maybe False ((== nextColour mycolour).fst) (board!on)

	move_targets :: PieceType -> [ (Coors,[Coors]) ]
	move_targets piecetype = case piecetype of
		Knight -> [ (s,[]) | s <- [
			north*2+east,north*2+west,east*2+north,east*2+south,
			south*2+east,south*2+west,west*2+north,west*2+south ] ]
		Bishop -> [ (s*(l,l), [ s*(i,i) | i <- [1..(l-1)] ]) | s <- diagonal, l <- [1..7] ]
		Rook   -> [ (s*(l,l), [ s*(i,i) | i <- [1..(l-1)] ]) | s <- straight, l <- [1..7] ]
		Queen  -> move_targets Bishop ++ move_targets Rook
		King   -> map (,[]) $ diagonal++straight

	straight@[south,north,east,west] = [(0,-1),(0,1),(1,0),(-1,0)]
	diagonal = [ north+east,north+west,south+east,south+west ]
	addrelcoors (file,rank) (dx,dy) = case ( file + dx, rank + dy ) of
		(x,y) | x `elem` [1..8] && y `elem` [1..8] -> Just (x,y)
		_ -> Nothing

	last_move = if length position == 0 then Nothing else Just (last position)

	(colour_to_move,_):_ = reverse $ zip coloursToMove $ map Just position ++ [Nothing]

putStrConsoleLn s = do
	putStrLn s
--	appendFile "test.txt" (s++"\n")

showPos board = do
	putStrConsoleLn $ "\xbf" ++ replicate 8 '\xc0' ++ "\xc1"
	forM_ [8,7..1] $ \ rank -> do
		line <- forM [1..8] $ \ file -> do
			return $ toEnum $ 0xd7 + mod (file+rank) 2 * 15 + case board!(file,rank) of
				Nothing             -> 0
				Just (colour,piece) -> 1 + fromEnum colour * 6 + fromEnum piece
		putStrConsoleLn $ [toEnum $ 0xc6 + rank ] ++ line ++ "\xc3"
	putStrConsoleLn $ "\xc4" ++ map (toEnum.(+0xce)) [1..8] ++ "\xc6"

testBoard = array ((1,1),(8,8)) $ zip [ (f,r) | r <- [8,7..1], f <- [1..8] ] [
	Nothing,Nothing ,Nothing ,Nothing,b King, Nothing ,Nothing ,b Rook ,
	Nothing,b Pawn  ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	w Pawn ,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	w Rook ,Nothing ,Nothing ,Nothing ,w King , Nothing,Nothing,w Rook ]
	where
	w = Just . (White,)
	b = Just . (Black,)


main = do
--	writeFile "test.txt" ""
	step [] testPosition

step moves board = do
	let board' = createBoard moves board
	forM_ (moveGenerator moves board') $ \ move -> do
		putStrConsoleLn "=================================="
		putStrConsoleLn $ show move
		showPos board
		print position
		s <- getLine
		case s of
			"" -> return ()
			"m" -> step $ position ++ [move]
		return ()