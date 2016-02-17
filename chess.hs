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
{--
	(from,Just (colour,piecetype)) <- assocs board,
	colour == colour_to_move,
	(to_rel,mb_take_rel,empties_d) <- let


		move_or_take (to_rel,empties) = case addrelcoors from to_rel of
			Nothing -> Nothing
			Just to -> case board!to of
				Nothing -> Just (to_rel,Nothing,to:empties)
				Just (other_colour,_) | other_colour/=colour -> Just (to_rel,Just to_rel,empties)
				_ -> Nothing

		can_take_rel on_rel = case addrelcoors from on_rel of
			Nothing -> False
			Just on -> can_take colour on

		not_moved coors = case board!coors of
			Nothing -> False
			Just _ -> all ((/=coors).moveFrom) moves

		rook_can_castle coors = case board!coors of
			Just (col,Rook) | col==colour_to_move -> not_moved coors
			_ -> False

		no_check coors = all ((/=coors).moveFrom) $ moveGenerator (Position moves board (nextColour colour_to_move))

		in case (piecetype,from),colour of
			_ -> catMaybes $ map move_or_take (move_targets piecetype) ++ case (piecetype,from,colour) of
				(King,(5,1),White) | not_moved (5,1) && no_check (5,1) ->
					(if rook_can_castle (8,1) && all no_check [(6,1),(7,1)] then Just (2*east,Nothing,[east,2*east]) else Nothing) :
					(if rook_can_castle (1,1) && all no_check [(4,1),(3,1)] then Just (2*east,Nothing,[west,2*west,3*west]) else Nothing) : []
				(King,(5,8),Black) | not_moved (5,8) && no_check (5,8) ->
					(if rook_can_castle (8,8) && all no_check [(6,8),(7,8)] then Just (2*east,Nothing,[east,2*east]) else Nothing) :
					(if rook_can_castle (1,8) && all no_check [(4,8),(3,8)] then Just (2*east,Nothing,[west,2*west,3*west]) else Nothing) : [],

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
--}

	where

	--can_take mycolour on = maybe False ((== nextColour mycolour).fst) (board!on)

	move_targets :: Colour -> [Coors]
	move_targets colour_to_move = [ move | (from,Just (colour,piecetype)) <- assocs board,
		colour==colour_to_move,
		targets <- concatMap (dir_targets from) $ case (piecetype,from) of
			(Pawn,(f,r)) -> (pawn_dir,if r==pawn_initial_file then 2 else 1)
				(if r==pawn_initial_file then [(pawn_dir*2,Nothing,[pawn_dir,pawn_dir*2])] else []) ++
				[ (pawn_dir+hor,Just (pawn_dir+hor),[]) | hor <- [east,west], can_take_rel (pawn_dir+hor) ] ++
				[ (pawn_dir+hor,Just hor,[]) | hor <- [east,west], can_take_rel hor,
					((Move last_from last_to Nothing Nothing):_) <- [reverse moves],
					Just (last_colour,Pawn) <- [board!last_to], nextColour last_colour == colour,
					last_from == from+2*pawn_dir+hor, last_to == from+hor ]
			Knight -> [ (s,[]) | s <- [
				north*2+east,north*2+west,east*2+north,east*2+south,
				south*2+east,south*2+west,west*2+north,west*2+south ] ]
			Bishop -> [ (s*(l,l), [ s*(i,i) | i <- [1..(l-1)] ]) | s <- diagonal, l <- [1..7] ]
			Rook   -> [ (s*(l,l), [ s*(i,i) | i <- [1..(l-1)] ]) | s <- straight, l <- [1..7] ]
			Queen  -> move_targets Bishop ++ move_targets Rook
			King   -> map (,[]) $ diagonal++straight
		]

		where

		(pawn_dir,pawn_initial_file) = if colour_to_move==White then (north,2) else (south,7)

		dir_targets _ _ _ (_,0) = []
		dir_targets from (moves,takes,direction,i) = case addrelcoors from direction of
			Nothing    -> []
			Just coors -> case board!coors of
				Just (colour,_) -> if colour == nextColour colour_to_move && takes then [coors] else []
				Nothing -> (if moves then [coors] else []) ++ dir_targets coors (moves,takes,direction,i-1)

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

