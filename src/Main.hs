{-# LANGUAGE TupleSections,ScopedTypeVariables,RecordWildCards,FlexibleContexts #-}

module Main where

import Data.Array
import Data.Maybe
import Data.List
import Control.Monad
import Text.Printf
import Data.NumInstances
import Data.Tuple
import Control.Monad.State.Strict
import Data.Char
import System.Time
import Debug.Trace

type File = Int
type Rank = Int
type Coors = (File,Rank)

type Depth = Int

data Colour = White | Black
	deriving (Eq,Show,Enum,Ord)

nextColour White = Black
nextColour Black = White
coloursToMove = iterate nextColour White

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
	deriving (Show,Eq,Enum,Ord)

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

data MatchEnding = Checkmate Colour | Stalemate Colour | Remis | MoveRepetition
	deriving (Eq,Show)

straight@[south,north,east,west] = [(0,-1),(0,1),(1,0),(-1,0)]
diagonal = [ north+east,north+west,south+east,south+west ]

pawnDir colour = if colour==White then north else south
pawnInitialRank colour = if colour==White then 2 else 7
pawnEnPassantRank colour = if colour==White then 5 else 4

type EndingOrMoves = Either MatchEnding [Move]

moveGenerator :: Position -> EndingOrMoves
moveGenerator position@(Position moves board colour_to_move) =
	case sort $ map swap $ catMaybes $ elems board of
		[ (King,_),(King,_) ] -> Left Remis
		[ (fig,_),(King,_),(King,_) ] | fig `elem` [Knight,Bishop] -> Left Remis
		[ (fig1,col1),(fig2,col2),(King,_),(King,_) ] | col1 /= col2 &&
			fig1 `elem` [Knight,Bishop] && fig2 `elem` [Knight,Bishop] -> Left Remis

		_ -> case filter (king_no_check position) $ [ Move from to mb_take mb_promotion |
				(piecetype,(from,(to,mb_take))) <- move_targets position,
				mb_promotion <- case (piecetype,to) of
					(Pawn,(_,r)) | r == 10 - pawnInitialRank colour_to_move -> map Just [Knight,Bishop,Rook,Queen]
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
			of
			[] -> Left $ case no_check position (kings_coors position) of
				True  -> Stalemate colour_to_move
				False -> Checkmate colour_to_move
			moves -> Right moves

	where

	castle_rank = if colour_to_move==White then 1 else 8

move_targets :: Position -> [(PieceType,(Coors,(Coors,Maybe Coors)))]
move_targets position@(Position moves board colour_to_move) = [ (piecetype,(from,target)) |
	(from,Just (colour,piecetype)) <- assocs board,
	colour==colour_to_move,
	target <- case (piecetype,from) of
		(Pawn,(_,r)) ->
			filter (isNothing.snd) (dir_targets from (pawn_dir,if r==pawn_initial_rank then 2 else 1)) ++
			filter (isJust.snd) (concatMap (dir_targets from) [(pawn_dir+west,1),(pawn_dir+east,1)]) ++
			[ (abs_to,Just take) | r == pawnEnPassantRank colour_to_move, eastwest <- [east,west],
				Just abs_to <- [ addrelcoors from (pawn_dir+eastwest) ],
				Nothing <- [ board!abs_to ],
				Just take <- [ addrelcoors from eastwest ],
				Just (col,Pawn) <- [ board!take ],
				Just pawn_from <- [ addrelcoors from (pawn_dir*2+eastwest) ],
				(Move last_from last_to Nothing Nothing):_ <- [ reverse moves ],
				last_from==pawn_from, last_to==take,
				col == nextColour colour_to_move ]
		_ -> concatMap (dir_targets from) $ case piecetype of
			Knight -> map (,1) [
				north*2+east,north*2+west,east*2+north,east*2+south,
				south*2+east,south*2+west,west*2+north,west*2+south ]
			Bishop -> map (,7) diagonal
			Rook   -> map (,7) straight
			Queen  -> map (,7) (straight++diagonal)
			King   -> map (,1) (straight++diagonal) ]
	where

	pawn_initial_rank = pawnInitialRank colour_to_move
	pawn_dir          = pawnDir colour_to_move

	dir_targets :: Coors -> (Coors,Int) -> [(Coors,Maybe Coors)]
	dir_targets _ (_,0) = []
	dir_targets from (direction,i) = case addrelcoors from direction of
		Nothing    -> []
		Just coors -> case board!coors of
			Just (colour,_) -> if colour == nextColour colour_to_move then [(coors,Just coors)] else []
			Nothing -> (coors,Nothing) : dir_targets coors (direction,i-1)

	addrelcoors (file,rank) (dx,dy) = case ( file + dx, rank + dy ) of
		(x,y) | x `elem` [1..8] && y `elem` [1..8] -> Just (x,y)
		_ -> Nothing

type Rating = Float
mAX_RATING = 1000000.0 :: Rating

evalPosition :: Position -> Rating
evalPosition pos@Position{..} = case moveGenerator pos of
	Left ending -> case ending of
		Checkmate colour -> (if colour==White then negate else id) mAX_RATING
		_                -> 0.0
	Right moves -> sum [ piece_val (coors,piece) | (coors,Just piece) <- assocs positionBoard ]
		where
		piece_val (coors@(f,r),(colour,piecetype)) = (if colour == White then id else negate) (
			case piecetype of
				Pawn   -> 1.0 + 0.1 * fromIntegral (6 - abs (r-pawn_targetrank)) + 0.10*num_moves
				Knight -> 3.0 + 0.10*proximity_to_centre +                         0.04*num_moves
				Bishop -> 3.0 + 0.10*proximity_to_centre +                         0.02*num_moves
				Rook   -> 5.0 +                                                    0.02*num_moves
				Queen  -> 9.0 + 0.10*proximity_to_centre +                         0.01*num_moves
				King   -> 10000.0 ) :: Rating
			where
			num_moves = fromIntegral (length (filter (==coors) $ map moveFrom moves))
			pawn_targetrank = if colour==White then 8 else 1
			proximity_to_centre = 5.0 - sqrt ( (abs (4.5 - fromIntegral r))^2 + (abs (4.5 - fromIntegral f))^2 )

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

{-
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
-}
testPosition = Position [] (array ((1,1),(8,8)) $ zip [ (f,r) | r <- [8,7..1], f <- [1..8] ] [
	Nothing,Nothing ,Nothing ,Nothing,b King, b Rook  ,Nothing ,w Rook ,
	Nothing,b Pawn  ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	w Pawn ,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	Nothing,Nothing ,Nothing ,Nothing,Nothing,Nothing ,Nothing ,Nothing,
	w Rook ,Nothing ,Nothing ,Nothing ,w King,Nothing ,Nothing, Nothing])
	Black
	where
	w = Just . (White,)
	b = Just . (Black,)

showFile f = take 1 $ drop f " abcdefgh"
showCoors (f,r) = showFile f ++ show r 

-- is the square coors not threatened by next player
no_check pos coors = all (\ (_,(_,(to,_))) -> coors /= to) $
	move_targets (pos { positionColourToMove = nextColour (positionColourToMove pos) })

-- the position of the king of the player to move in this position
kings_coors Position{..} = head [ coors | (coors,Just (col,King)) <- assocs positionBoard, col==positionColourToMove ]

-- would the king of the player to move in pos be in check after the move
king_no_check pos move = no_check pos' (kings_coors pos') where
	pos' = (doMove pos move) { positionColourToMove = positionColourToMove pos }

showMove :: Position -> Move -> String
showMove pos@Position{..} move@(Move from@(f0,r0) to mb_takes mb_promote) =
	case (piecetype_at from,from,to) of
		(King,(5,_),(7,_)) -> "O-O"
		(King,(5,_),(3,_)) -> "O-O-O"
		(piece_from,_,_) -> ( case mb_takes of
			Just takes | takes/=to || piece_from == Pawn -> showFile f0
			_ ->
				pieceStr piece_from ++
				head ([ if f==f0 then show r0 else showFile f0 |
					Move from'@(f,r) to' _ _ <- moves, from' /= from, to==to',
					piece_from == piecetype_at from' ] ++ [""]) ) ++
			(if isJust mb_takes then "x" else "") ++
			showCoors to ++
			(maybe "" pieceStr mb_promote) ++
			(if no_check pos' (kings_coors pos') then "" else (
				if moveGenerator pos' == Left (Checkmate (nextColour positionColourToMove))
					then "#" else "+"))
	where
	Right moves = moveGenerator pos
	piecetype_at coors = snd $ fromJust (positionBoard!coors)
	pos' = doMove pos move

pieceStr piecetype = fromJust $ lookup piecetype [(Pawn,""),(Knight,"N"),(Bishop,"B"),(Rook,"R"),(Queen,"Q"),(King,"K")]

showMoves :: Position -> [Move] -> IO ()
showMoves pos moves = showline moves where
	showline [] = return ()
	showline ms = do
		putStrConsoleLn $ concatMap (\ m -> printf "%-8s" (showMove pos m)) (take 10 ms)
		showline (drop 10 ms)

main = do
--	writeFile "test.txt" ""
	loop 2 testPosition

loop depth pos = do
	putStrConsoleLn "\n==================================\n"
	showPos pos
	putStrConsoleLn $ printf "\nRating = %+2.2f" (evalPosition pos)
	putStrConsoleLn $ "Current search depth setting = " ++ show depth
	case moveGenerator pos of
		Left ending -> do
			putStrConsoleLn $ "ENDING: " ++ show ending
			putStrConsoleLn "ENTER to take back, if possible"
			getLine
			return ()
		Right moves -> do
			putStrConsoleLn "\nPossible moves:"
			showMoves pos moves
			putStrConsoleLn $ intercalate "  " (map showMove_FromTo moves)
			putStrConsoleLn "\nEnter command:\n> "
			s <- getLine
			case s of
				"s" -> do
					putStrConsoleLn "Searching..."
					((val,line),s) <- runStateT (search depth pos) initialSearchState
					case line of
						[] -> putStrConsoleLn $ printf "Value = %+2.2f, no move possible." val
						best_move:_ -> do
							putStrConsoleLn $ showSearchState s
							putStrConsoleLn $ "Found best move " ++ showMove pos best_move
							putStrConsoleLn $ "Best line: " ++ showLine line
							do_move best_move
				"b" -> return ()
				"q" -> error $ "Quit."
				depthstr | length depthstr > 0 && all isDigit depthstr -> do
					let (depth',""):_ = (reads :: ReadS Int) depthstr
					putStrConsoleLn $ "Setting depth = " ++ show depth'
					loop depth' pos
				movestr -> case filter ((==movestr).showMove_FromTo) moves of
					[move] -> do_move move
					_ -> do
						putStrConsoleLn $ "Wrong move: " ++ show movestr
						loop depth pos
	where

	do_move	move = do
		loop depth (doMove pos move)
		loop depth pos

showMove_FromTo Move{..} = showCoors moveFrom ++ showCoors moveTo ++ maybe "" pieceStr movePromote

showLine :: [Move] -> String
showLine moves = intercalate ", " (map showMove_FromTo moves)

data SearchState = SearchState {
	debugMode       :: Bool,
	nodesProcessed  :: Int,
	leavesProcessed :: Int,
	evaluationsDone :: Int,
	bestLineUpdates :: Int,
	lastStateOutputTime :: Integer,
	bestLine            :: [Move],
	bestVal             :: Rating,
	computationProgress :: [(Int,Int)] }
	deriving (Show)
initialSearchState = SearchState False 0 0 0 0 0 [] 0.0 []

showSearchState s = printf "Tot. %i Nodes, %i Leaves, %i Evals, bestLineUpdates=%i"
	(nodesProcessed s) (leavesProcessed s) (evaluationsDone s) (bestLineUpdates s)

comp_progress _ [] = 0.0 :: Float
comp_progress ts ((i,n):ins) = product ts * fromIntegral i / fromIntegral n + comp_progress ((1 / fromIntegral n):ts) ins

type SearchMonad a = StateT SearchState IO a

search :: Depth -> Position -> SearchMonad (Rating,[Move])
search maxdepth position = do_search maxdepth 0 position [] (-mAX_RATING,mAX_RATING)

debug_here depth str current_line alphabeta = do
	s <- get
	when (debugMode s) $ do
		input <- liftIO $ do
			putStrConsoleLn $ "\n========== " ++ str
			putStrConsoleLn $ printf "Depth=%i, Best Rating=%+.2f" depth (bestVal s)
			putStrConsoleLn $ "Best Line: " ++ showLine (bestLine s)
			putStrConsoleLn $ "Computation Progress: " ++ show (computationProgress s)
			putStrConsoleLn $ "Current Line: " ++ showLine current_line
			putStrConsoleLn $ "(alpha,beta) = " ++ show alphabeta
			putStrConsoleLn $ "============================"
			putStrConsoleLn "Press Enter or 'd0'"
			getLine
		case input of
			"d0" -> modify' $ \ s -> s { debugMode = False }
			_ -> return ()

do_search :: Depth -> Depth -> Position -> [Move] -> (Rating,Rating) -> SearchMonad (Rating,[Move])
do_search maxdepth depth position current_line alphabeta = 
	case moveGenerator position of
		Left _ -> return (evalPosition position,[])
		Right [] -> error "The impossible happened: Move generator returned empty move list!"
		Right moves -> do
			modify' $ \ s -> s { computationProgress = (0,length moves) : computationProgress s }
			res <- find_best_line (worst_val,[]) moves
			debug_here depth ("find_best_line returned " ++ show res) current_line alphabeta
			modify' $ \ s -> s { computationProgress = tail (computationProgress s) }
			debug_here depth "AFTER FIND_BEST_LINE" current_line alphabeta
			return res

		where

		(worst_val,isBetterThan) = case positionColourToMove position of
			White -> (-mAX_RATING,(>))
			Black -> ( mAX_RATING,(<))

		find_best_line :: (Rating,[Move]) -> [Move] -> SearchMonad (Rating,[Move])
		find_best_line best [] = do
			debug_here depth ("find_best_line [] returned " ++ show best) current_line alphabeta
			return best
		find_best_line best@(best_val,best_line) (move:moves) = do
			let
				current_line' = current_line ++ [move]
				depth' = depth + 1
				position' = doMove position move
			debug_here depth' ("CURRENT MOVE: " ++ showMove_FromTo move) current_line' alphabeta
			modify' $ \ s -> s { nodesProcessed = nodesProcessed s + 1 }
			(this_val,this_subline) <- case depth' < maxdepth of
				True -> do_search maxdepth depth' position' current_line' alphabeta
				False -> do
					modify' $ \ s -> s {
						leavesProcessed = leavesProcessed s + 1,
						nodesProcessed  = nodesProcessed  s + 1,
						evaluationsDone = evaluationsDone s + 1 }
					last_output_secs <- gets lastStateOutputTime
					TOD current_secs _ <- liftIO $ getClockTime
					when (current_secs - last_output_secs >=1) $ do
						s <- get
						liftIO $ putStrConsoleLn $ printf "[%3.0f%%] Best line: %+.2f  <-  %s"
							(100.0 * comp_progress [] (reverse $ computationProgress s)) (bestVal s) (showLine (bestLine s))
						modify' $ \ s -> s { lastStateOutputTime = current_secs }
					return $ (evalPosition position',[])			
			best' <- case this_val `isBetterThan` best_val of
				True -> do
					modify' $ \ s -> s {
						bestLine = current_line' ++ this_subline,
						bestVal = this_val,
						bestLineUpdates = bestLineUpdates s + 1 }
					return (this_val,move:this_subline)
				False -> return best
			modify' $ \ s -> s { computationProgress = let ((i,n):ps) = computationProgress s in (i+1,n):ps }
			debug_here depth' (printf "AFTER COMPARISON: BEST was %+.2f, THIS is %+.2f" best_val this_val) current_line' alphabeta
			find_best_line best' moves
