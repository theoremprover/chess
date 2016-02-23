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

data Position = Position [Move] Board Colour
initialPosition = Position [] initialBoard White

data Move = Move {
	moveFrom :: Coors, moveTo :: Coors, moveTakes :: Maybe Coors, movePromote :: Maybe PieceType }
	deriving (Eq,Show)

doMove :: Position -> Move -> Position
doMove (Position moves board colour) move@(Move from to mb_take mb_promotion) =
	Position (moves++[move]) (board // (
		maybe [] ((:[]).(,Nothing)) mb_take ++
		(from,Nothing) :
		(to,case mb_promotion of
			Nothing       -> board!from
			Just promoted -> Just (my_colour,promoted) where
				Just (my_colour,_) = board!from ) :
		case (from,to,board!from) of
			((5,r),(7,_),(Just (colour,King))) -> [ ((8,r),Nothing),((6,r),Just (colour,Rook)) ]
			((5,r),(3,_),(Just (colour,King))) -> [ ((1,r),Nothing),((4,r),Just (colour,Rook)) ]
			_ -> [] ))
		(nextColour colour)

moveGenerator position@(Position moves board colour_to_move) = [ Move from to mb_take mb_promotion |
	(piecetype,from,(to,mb_take)) <- move_targets position,
	mb_promotion <- case (piecetype,to) of
		(Pawn,(_,r)) | r == 10 - pawn_initial_rank -> map Just [Knight,Bishop,Rook,Queen]
		_ -> [Nothing] ] ++
	[ Move from to Nothing Nothing |
		
	where

	castle_rank = if 

	move_targets :: Position -> [(PieceType,Coors,(Coors,Maybe Coors))]
	move_targets (Position moves board colour_to_move) = [ (piecetype,from,target) | (from,Just (colour,piecetype)) <- assocs board,
		colour==colour_to_move,
		target <- case (piecetype,from) of
			(Pawn,(f,r)) ->
				filter (isNothing.snd) (dir_targets (pawn_dir,if r==pawn_initial_rank then 2 else 1)) ++
				filter (isJust.snd) (concatMap $ dir_targets [(pawn_dir+west,1),(pawn_dir+east,1)]) ++
				[ (pawn_dir+eastwest,Just take) | r == 9 - pawn_initial_rank, eastwest <- [east,west],
					Just takecoors <- addrelcoors from eastwest, Just (col,Pawn) <- board!takecoors,
					col == nextColour colour_to_move,
					(Move last_from last_to _ _):_ <- reverse moves ]
			_ -> concatMap (dir_targets from) $ case piecetype of
				Knight -> map (,1) [
					north*2+east,north*2+west,east*2+north,east*2+south,
					south*2+east,south*2+west,west*2+north,west*2+south ]
				Bishop -> map (,7) diagonal
				Rook   -> map (,7) straight
				Queen  -> map (,7) straight++diagonal
				King   -> map (,1) straight++diagonal

	(pawn_dir,pawn_initial_rank) = if colour_to_move==White then (north,2) else (south,7)

	dir_targets _ (_,0) = []
	dir_targets from (direction,i) = case addrelcoors from direction of
		Nothing    -> []
		Just coors -> case board!coors of
			Just (colour,_) -> if colour == nextColour colour_to_move then [(coors,Just coors)] else []
			Nothing -> (coors,Nothing) : dir_targets coors (direction,i-1)

	straight@[south,north,east,west] = [(0,-1),(0,1),(1,0),(-1,0)]
	diagonal = [ north+east,north+west,south+east,south+west ]

	addrelcoors (file,rank) (dx,dy) = case ( file + dx, rank + dy ) of
		(x,y) | x `elem` [1..8] && y `elem` [1..8] -> Just (x,y)
		_ -> Nothing

putStrConsoleLn s = do
	putStrLn s
--	appendFile "test.txt" (s++"\n")

showPos (Position moves board colour) = do
	putStrConsoleLn $ "\xbf" ++ replicate 8 '\xc0' ++ "\xc1"
	forM_ [8,7..1] $ \ rank -> do
		line <- forM [1..8] $ \ file -> do
			return $ toEnum $ 0xd7 + mod (file+rank) 2 * 15 + case board!(file,rank) of
				Nothing             -> 0
				Just (colour,piece) -> 1 + fromEnum colour * 6 + fromEnum piece
		putStrConsoleLn $ [toEnum $ 0xc6 + rank ] ++ line ++ "\xc3"
	putStrConsoleLn $ "\xc4" ++ map (toEnum.(+0xce)) [1..8] ++ "\xc6"
	putStrConsoleLn $ show colour ++ " to move"

testPosition = Position [] (array ((1,1),(8,8)) $ zip [ (f,r) | r <- [8,7..1], f <- [1..8] ] [
	Nothing,Nothing ,Nothing ,Nothing,b King, Nothing ,Nothing ,b Rook ,
	Nothing,b Pawn  ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	w Pawn ,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	w Rook ,Nothing ,Nothing ,Nothing ,w King,Nothing ,Nothing, w Rook ])
	Black
	where
	w = Just . (White,)
	b = Just . (Black,)

main = do
--	writeFile "test.txt" ""
	step testPosition (moveGenerator testPosition)

step _ [] = return ()
step position (move:left_moves)= do
	putStrConsoleLn "=================================="
	putStrConsoleLn $ show move
	let pos' = doMove position move
	showPos pos'
	s <- getLine
	case s of
		""  -> step position left_moves
		"m" -> step (doMove position move) left_moves
		"b" -> return ()
