{-# LANGUAGE TupleSections,ScopedTypeVariables,RecordWildCards #-}

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

data Position = Position {
	positionMoves :: [Move], positionBoard::Board, positionColourToMove::Colour }
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

moveGenerator position@(Position moves board colour_to_move) = filter king_no_check $
	[ Move from to mb_take mb_promotion |
		(piecetype,(from,(to,mb_take))) <- move_targets position,
		mb_promotion <- case (piecetype,to) of
			(Pawn,(_,r)) | r == 10 - pawn_initial_rank -> map Just [Knight,Bishop,Rook,Queen]
			_ -> [Nothing] ] ++
		( case map ((board!).(,castle_rank)) [5..8] of
			[ Just (kingcol,King),Nothing,Nothing,Just (rookcol,Rook) ] |
				kingcol==colour_to_move && rookcol==colour_to_move &&
				all (no_check position) (map (,castle_rank) [5..6]) &&
				all (\ (Move f _ _ _) -> f /= (5,castle_rank) && f /= (8,castle_rank)) moves ->
					[ Move (5,castle_rank) (7,castle_rank) Nothing Nothing ]
			_ -> [] ) ++
		( case map ((board!).(,castle_rank)) [1..5] of
			[ Just (rookcol,Rook),Nothing,Nothing,Nothing,Just (kingcol,King) ] |
				kingcol==colour_to_move && rookcol==colour_to_move &&
				all (no_check position) (map (,castle_rank) [4..5]) &&
				all (\ (Move f _ _ _) -> f /= (1,castle_rank) && f /= (5,castle_rank)) moves ->
					[ Move (5,castle_rank) (3,castle_rank) Nothing Nothing ]
			_ -> [] )

	where

	king_no_check move = all (no_check pos') [ coors |
		(coors,Just (col,King)) <- assocs (positionBoard pos'), col==colour_to_move ]
		where
		pos' = (doMove position move) { positionColourToMove = colour_to_move }

	castle_rank = if colour_to_move==White then 1 else 8

	no_check pos coors = all (\ (_,(_,(to,_))) -> coors /= to) $
		move_targets (pos { positionColourToMove = nextColour (positionColourToMove pos) })

	move_targets :: Position -> [(PieceType,(Coors,(Coors,Maybe Coors)))]
	move_targets position@(Position moves board colour_to_move) = [ (piecetype,(from,target)) |
		(from,Just (colour,piecetype)) <- assocs board,
		colour==colour_to_move,
		target <- case (piecetype,from) of
			(Pawn,(_,r)) ->
				filter (isNothing.snd) (dir_targets from (pawn_dir,if r==pawn_initial_rank then 2 else 1)) ++
				filter (isJust.snd) (concatMap (dir_targets from) [(pawn_dir+west,1),(pawn_dir+east,1)]) ++
				[ (pawn_dir+eastwest,Just take) | r == 9 - pawn_initial_rank, eastwest <- [east,west],
					Just take <- [ addrelcoors from eastwest ],
					Just (col,Pawn) <- [ board!take ],
					Just pawn_from <- [ addrelcoors from (pawn_dir*2+eastwest) ],
					(Move last_from last_to _ _):_ <- [ reverse moves ], last_from==pawn_from, last_to==take,
					col == nextColour colour_to_move ]
			_ -> concatMap (dir_targets from) $ case piecetype of
				Knight -> map (,1) [
					north*2+east,north*2+west,east*2+north,east*2+south,
					south*2+east,south*2+west,west*2+north,west*2+south ]
				Bishop -> map (,7) diagonal
				Rook   -> map (,7) straight
				Queen  -> map (,7) (straight++diagonal)
				King   -> map (,1) (straight++diagonal) ]

	(pawn_dir,pawn_initial_rank) = if colour_to_move==White then (north,2) else (south,7)

	dir_targets :: Coors -> (Coors,Int) -> [(Coors,Maybe Coors)]
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

ratePosition Position{..} = 
	sum [ piece_val (coors,piece) | (coors,Just piece) <- assocs positionBoard ]
	where
	piece_val (coors@(f,r),(colour,piecetype)) pos = (if colour == White then 1.0 else -1.0) *
		case piecetype of
			Pawn   -> 1.0 + 0.1 * fromIntegral (6 - abs (r-pawn_targetrank))
			Knight -> 3.0 + 0.1 * proximity_to_centre
			Bishop -> 3.0 + 0.1 * proximity_to_centre
			Rook   -> 5.0
			Queen  -> 9.0 + 0.1 * proximity_to_centre
			King   -> 10000.0
		where
		pawn_targetrank = if colour==White then 8 else 1
		proximity_to_centre = 5 - sqrt $ (abs (4.5 - fromIntegral r))^2 + (abs (4.5 - fromIntegral f))^2

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
	Nothing,Nothing ,Nothing ,Nothing,b King, b Rook  ,Nothing ,w Rook ,
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
	putStrConsoleLn $ "After " ++ show move ++ ":"
	showPos (doMove position move)
	s <- getLine
	case s of
		""  -> step position left_moves
		"m" -> do
			let pos' = doMove position move
			step pos' (moveGenerator pos')
		"b" -> return ()

