{-# LANGUAGE TupleSections,ScopedTypeVariables,RecordWildCards,FlexibleContexts,UnicodeSyntax #-}

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
import Data.Ord
import System.Time
import qualified Data.IntMap.Strict as IntMap

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU

type File = Int
type Rank = Int
type Coors = (File,Rank)

type Depth = Int

data Colour = White | Black
	deriving (Eq,Show,Enum,Ord)

nextColour White = Black
nextColour Black = White
coloursToMove = iterate nextColour White

data PieceType = Ù | Ú | Û | Ü | Ý | Þ
	deriving (Show,Eq,Enum,Ord,Ix)

type Piece = (Colour,PieceType)

type Board = Array Coors (Maybe Piece)

stringToPosition :: Colour -> [String] -> Position
stringToPosition col_to_move s = Position (array ((1,1),(8,8)) $
	zip [ (f,r) | r <- [8,7..1], f <- [1..8] ] (map tofig (concat s)))
	col_to_move (const True) (const True) Nothing 0 0
	where
	tofig c | c >= 'ç' = tofig (chr $ ord c - ord 'ç' + ord 'Ø')
	tofig 'Ø' = Nothing
	tofig c = Just (toEnum (div i 6),toEnum (mod i 6)) where
		i = ord c - ord 'Ù'

testPosition = stringToPosition Black [
	"çØçØóâçÜ",
	"ØîØçØçØç",
	"çØçØçØçØ",
	"ÙçØçØçØç",
	"çØçØçØçØ",
	"ØçØçØçØç",
	"çØçØçØçØ",
	"ÜçØçÞçØç" ]

initialPosition = stringToPosition White [
	"âïáòäðàñ",
	"îßîßîßîß",
	"ØçØçØçØç",
	"çØçØçØçØ",
	"ØçØçØçØç",
	"çØçØçØçØ",
	"ÙèÙèÙèÙè",
	"ëÚêÝíÛéÜ" ]

data Position = Position {
	positionBoard              :: Board,
	positionColourToMove       :: Colour,
	positionCanCastleQueenSide :: Colour -> Bool,
	positionCanCastleKingSide  :: Colour -> Bool,
	positionEnPassantSquare    :: Maybe Coors,
	positionHalfmoveClock      :: Int,
	positionMoveCounter        :: Int }

data Move = Move {
	moveFrom :: Coors, moveTo :: Coors, moveTakes :: Maybe Coors, movePromote :: Maybe PieceType }
	deriving (Eq,Show)

doMove :: Position -> Move -> Position
doMove (Position board colour castlequeen castleking mb_ep halfmoves movecounter) move@(Move from to mb_take mb_promotion) =
	Position (board // (
		maybe [] ((:[]).(,Nothing)) mb_take ++
		(from,Nothing) :
		(to,case mb_promotion of
			Nothing       -> board!from
			Just promoted -> Just (my_colour,promoted)) :
		add_dels ))
		(nextColour colour)
		(makecastlef castlequeen $ castlequeen my_colour && not castled_queen && from `notElem` [(5,castle_rank),(1,castle_rank)])
		(makecastlef castleking  $ castleking  my_colour && not castled_king  && from `notElem` [(5,castle_rank),(8,castle_rank)])
		( case (moved_piecetype,from,to) of
			(Ù,(r,f0),(_,f1)) | abs (f1-f0) == 2 -> Just (r,(div (f0+f1) 2))
			_ -> Nothing )
		(if isJust mb_take || moved_piecetype==Ù then 0 else halfmoves+1)
		(if colour==Black then movecounter+1 else movecounter)
	where
	castle_rank = castleRank my_colour
	makecastlef f' val col = if col==colour then val else f' col
	Just (my_colour,moved_piecetype) = board!from
	(castled_queen,castled_king,add_dels) = case (from,to,board!from) of
		((5,r),(7,_),(Just (col,Þ))) | col==my_colour -> (False,True,[ ((8,r),Nothing),((6,r),Just (col,Ü)) ])
		((5,r),(3,_),(Just (col,Þ))) | col==my_colour -> (True,False,[ ((1,r),Nothing),((4,r),Just (col,Ü)) ])
		_ -> (False,False,[])

data MatchEnding = Checkmate Colour | Stalemate Colour | Remis | MoveRepetition
	deriving (Eq,Show)

straight@[south,north,east,west] = [(0,-1),(0,1),(1,0),(-1,0)]
diagonal = [ north+east,north+west,south+east,south+west ]

pawnDir colour = if colour==White then north else south
pawnInitialRank colour = if colour==White then 2 else 7
pawnEnPassantRank colour = if colour==White then 5 else 4
castleRank colour = if colour==White then 1 else 8

moveGenerator :: Position -> Either MatchEnding [Move]
moveGenerator position@(Position board colour_to_move cancastlequeen cancastleking mb_epsquare halfmoves _) =
	case sort $ map swap $ catMaybes $ elems board of
		[ (Þ,_),(Þ,_) ] -> Left Remis
		[ (f,_),(Þ,_),(Þ,_) ] | f `elem` [Ú,Û] -> Left Remis
		[ (f,col1),(g,col2),(Þ,_),(Þ,_) ] | col1 /= col2 &&
			f `elem` [Ú,Û] && g `elem` [Ú,Û] -> Left Remis
		_ | halfmoves >= 50 -> Left Remis
		_ -> case filter (king_no_check position) $ [ Move from to mb_take mb_promotion |
			(piecetype,(from,(to,mb_take))) <- move_targets position,
			mb_promotion <- case (piecetype,to) of
				(Ù,(_,r)) | r == 10 - pawnInitialRank colour_to_move -> map Just [Ú,Û,Ü,Ý]
				_ -> [Nothing] ] ++
			if cancastleking colour_to_move then
				case map ((board!).(,castle_rank)) [5..8] of
					[ Just (kingcol,Þ),Nothing,Nothing,Just (rookcol,Ü) ] |
						kingcol==colour_to_move && rookcol==colour_to_move &&
						all (no_check position) [(5,castle_rank),(6,castle_rank)] ->
							[ Move (5,castle_rank) (7,castle_rank) Nothing Nothing ]
					_ -> []
				else []
			++
			if cancastlequeen colour_to_move then
				case map ((board!).(,castle_rank)) [1..5] of
					[ Just (rookcol,Ü),Nothing,Nothing,Nothing,Just (kingcol,Þ) ] |
						kingcol==colour_to_move && rookcol==colour_to_move &&
						all (no_check position) [(4,castle_rank),(5,castle_rank)] ->
							[ Move (5,castle_rank) (3,castle_rank) Nothing Nothing ]
					_ -> []
				else []
			of
			[] -> Left $ (if no_check position (kings_coors position) then Stalemate else Checkmate) colour_to_move
			moves -> Right moves

	where

	castle_rank = castleRank colour_to_move

move_targets :: Position -> [(PieceType,(Coors,(Coors,Maybe Coors)))]
move_targets position@(Position board colour_to_move _ _ mb_ep _ _) = [ (piecetype,(from,target)) |
	(from,Just (colour,piecetype)) <- assocs board,
	colour==colour_to_move,
	target <- case (piecetype,from) of
		(Ù,(_,r)) ->
			filter (isNothing.snd) (dir_targets from (pawn_dir,if r==pawn_initial_rank then 2 else 1)) ++
			filter (isJust.snd) (concatMap (dir_targets from) [(pawn_dir+west,1),(pawn_dir+east,1)]) ++
			[ (abs_to,Just take) | r == pawnEnPassantRank colour_to_move, eastwest <- [east,west],
				Just abs_to <- [ addrelcoors from (pawn_dir+eastwest) ],
				Nothing <- [ board!abs_to ],
				Just take <- [ addrelcoors from eastwest ],
				Just take == mb_ep,
				Just (col,Ù) <- [ board!take ],
				Just pawn_from <- [ addrelcoors from (pawn_dir*2+eastwest) ],
				col == nextColour colour_to_move ]
		_ -> concatMap (dir_targets from) $ case piecetype of
			Ú -> map (,1) [
				north*2+east,north*2+west,east*2+north,east*2+south,
				south*2+east,south*2+west,west*2+north,west*2+south ]
			Û -> map (,7) diagonal
			Ü -> map (,7) straight
			Ý -> map (,7) (straight++diagonal)
			Þ -> map (,1) (straight++diagonal) ]
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
				Ù -> 1.0 + 0.1 * fromIntegral (6 - abs (r-pawn_targetrank)) + 0.10*num_moves
				Ú -> 3.0 + 0.10*proximity_to_centre +                         0.04*num_moves
				Û -> 3.0 + 0.10*proximity_to_centre +                         0.02*num_moves
				Ü -> 5.0 +                                                    0.02*num_moves
				Ý -> 9.0 + 0.05*proximity_to_centre +                         0.01*num_moves
				Þ -> 10000.0 ) :: Rating
			where
			num_moves = fromIntegral (length (filter (==coors) $ map moveFrom moves))
			pawn_targetrank = if colour==White then 8 else 1
			proximity_to_centre = 5.0 - sqrt ( (abs (4.5 - fromIntegral r))^2 + (abs (4.5 - fromIntegral f))^2 )

putStrConsole s = do
	putStr s
--	BSC.putStrLn $ BSU.fromString s
--	appendFile "test.txt" (s++"\n")

putStrConsoleLn s = putStrConsole $ s ++ "\n"

toFEN :: Position -> String
toFEN (Position board colour castlequeen castleking mb_ep halfmove_clock movecounter) =
	intercalate "/" [ row2fen 0 [ board!(f,r) | f <- [1..8] ] | r <- [8,7..1] ] ++ " " ++
	(if colour==White then "w" else "b") ++ " " ++
	(if castles=="" then "-" else castles) ++ " " ++
	maybe "-" showCoors mb_ep ++ " " ++
	show halfmove_clock ++ " " ++ show movecounter
	where
	castles =
		(if castleking  White then "K" else "") ++
		(if castlequeen White then "Q" else "") ++
		(if castleking  Black then "k" else "") ++
		(if castlequeen Black then "q" else "")
	row2fen :: Int -> [Maybe (Colour,PieceType)] -> String
	row2fen 0 [] = ""
	row2fen cnt [] = show cnt
	row2fen cnt (Nothing:rs) = row2fen (cnt+1) rs
	row2fen cnt ((Just (col,piecetype)) : rs) = (if cnt>0 then show cnt else "") ++
		map (if col==White then toUpper else toLower) (if piecetype==Ù then "p" else pieceStr piecetype) ++
		row2fen 0 rs

showPos pos@(Position board colour _ _ _ _ _) = do
	putStrConsoleLn $ "¿" ++ replicate 8 'À' ++ "Á"
	forM_ [8,7..1] $ \ rank -> do
		line <- forM [1..8] $ \ file -> do
			return $ toEnum $ 0xd8 + mod (file+rank) 2 * 15 + case board!(file,rank) of
				Nothing             -> 0
				Just (colour,piece) -> 1 + fromEnum colour * 6 + fromEnum piece
		putStrConsoleLn $ [toEnum $ 0xc6 + rank ] ++ line ++ "Ã"
	putStrConsoleLn $ "Ä" ++ map (toEnum.(+0xce)) [1..8] ++ "Æ"
	putStrConsoleLn $ "FEN: " ++ toFEN pos
	putStrConsoleLn $ show colour ++ " to move"

showFile f = [ "abcdefgh" !! (f-1) ]
showCoors (f,r) = showFile f ++ show r 

-- is the square coors not threatened by next player
no_check pos coors = all (\ (_,(_,(to,_))) -> coors /= to) $
	move_targets (pos { positionColourToMove = nextColour (positionColourToMove pos) })

-- the position of the king of the player to move in this position
kings_coors Position{..} = head [ coors | (coors,Just (col,Þ)) <- assocs positionBoard, col==positionColourToMove ]

-- would the king of the player to move in pos be in check after the move
king_no_check pos move = no_check pos' (kings_coors pos') where
	pos' = (doMove pos move) { positionColourToMove = positionColourToMove pos }

showMove :: Position -> Move -> String
showMove pos@Position{..} move@(Move from@(f0,r0) to mb_takes mb_promote) =
	case (piecetype_at from,from,to) of
		(Þ,(5,_),(7,_)) -> "O-O"
		(Þ,(5,_),(3,_)) -> "O-O-O"
		(piece_from,_,_) -> ( case mb_takes of
			Just takes | takes/=to || piece_from == Ù -> showFile f0
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

pieceStr piecetype = fromJust $ lookup piecetype [(Ù,""),(Ú,"N"),(Û,"B"),(Ü,"R"),(Ý,"Q"),(Þ,"K")]

showMoves :: Position -> [Move] -> IO ()
showMoves pos moves = showline moves where
	showline [] = return ()
	showline ms = do
		putStrConsoleLn $ concatMap (\ m -> printf "%-8s" (showMove pos m)) (take 10 ms)
		showline (drop 10 ms)

main = do
--	writeFile "test.txt" ""
	putStrConsoleLn "Cutoffs  ëÚêÝíÛéÜ"
	loop 4 testPosition

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
--			showMoves pos moves
			putStrConsoleLn $ intercalate "  " (map showMove_FromTo moves)
			putStrConsole "\nEnter command:\n> "
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
	αCutoffs            :: Int,
	βCutoffs            :: Int,
	computationProgress :: [(Int,Int)],
	killerMoveHits      :: Int }
	deriving (Show)
initialSearchState = SearchState False 0 0 0 0 0 [] 0.0 0 0 [] 0

showSearchState s = printf "Tot. %i Nodes, %i Leaves, %i Evals, bestLineUpdates=%i, alpha/beta-Cutoffs=%i/%i"
	(nodesProcessed s) (leavesProcessed s) (evaluationsDone s) (bestLineUpdates s) (αCutoffs s) (βCutoffs s)

comp_progress _ [] = 0.0 :: Float
comp_progress ts ((i,n):ins) = product ts * fromIntegral i / fromIntegral n + comp_progress ((1 / fromIntegral n):ts) ins

type SearchMonad a = StateT SearchState IO a

search :: Depth -> Position -> SearchMonad (Rating,[Move])
search maxdepth position = do
	(res,_) <- do_search maxdepth 0 position [] (-mAX_RATING,mAX_RATING) IntMap.empty
	return res

debug_here depth str current_line αβ = do
	s <- get
	when (debugMode s) $ do
		input <- liftIO $ do
			putStrConsoleLn $ "\n========== " ++ str
			putStrConsoleLn $ printf "Depth=%i, Best Rating=%+.2f" depth (bestVal s)
			putStrConsoleLn $ "Best Line: " ++ showLine (bestLine s)
			putStrConsoleLn $ "Computation Progress: " ++ show (computationProgress s)
			putStrConsoleLn $ "Current Line: " ++ showLine current_line
			putStrConsoleLn $ "(alpha,beta) = " ++ show αβ
			putStrConsoleLn $ printf "alphaCutoffs = %i, betaCutoffs = %i" (αCutoffs s) (βCutoffs s)
			putStrConsoleLn $ "============================"
			putStrConsoleLn "Press Enter or 'd0'"
			getLine
		case input of
			"d0" -> modify' $ \ s -> s { debugMode = False }
			_ -> return ()

type KillerMoves = IntMap.IntMap [Move]

numKillerMoves = 5
uniteKillerMoves ms1 ms2 = take numKillerMoves (union ms1 ms2)

do_search :: Depth -> Depth -> Position -> [Move] -> (Rating,Rating) -> KillerMoves -> SearchMonad ((Rating,[Move]),KillerMoves)
do_search maxdepth depth position current_line (α,β) killermoves =
	case moveGenerator position of
		Left  _  -> return ((evalPosition position,[]),killermoves)
		Right unsorted_moves -> do
			let
				presorted_moves = reverse $ sortBy (comparing move_sort_val) unsorted_moves
				kill_moves = (IntMap.findWithDefault [] depth killermoves) `intersect` presorted_moves
				moves = kill_moves ++ (presorted_moves \\ kill_moves)
			modify' $ \ s -> s { computationProgress = (0,length moves) : computationProgress s }
			(res,killer_moves') <- find_best_line (worst_val,[]) moves (α,β) killermoves
			debug_here depth ("find_best_line returned " ++ show res) current_line (α,β)
			modify' $ \ s -> s { computationProgress = tail (computationProgress s) }
			debug_here depth "AFTER FIND_BEST_LINE" current_line (α,β)
			return (res,killer_moves')

		where

		move_sort_val (Move from to mb_takes mb_promote) =
			maybe 0 (\ c -> max 1 (piecetypeval_at c - piecetypeval_at from)) mb_takes +
			maybe 0 (const 10) mb_promote
			where
			piecetypeval_at coors = let Just (_,piecetype) = (positionBoard position)!coors in 1 + index (Ù,Þ) piecetype

		(worst_val,isBetterThan,accum_fun) = case positionColourToMove position of
			White -> (α,(>),max)
			Black -> (β,(<),min)

		find_best_line :: (Rating,[Move]) -> [Move] -> (Rating,Rating) -> KillerMoves -> SearchMonad ((Rating,[Move]),KillerMoves)
		find_best_line best@(best_val,best_line) (move:moves) (α,β) killermoves = do
			let
				current_line' = current_line ++ [move]
				depth' = depth + 1
				position' = doMove position move
			debug_here depth' ("CURRENT MOVE: " ++ showMove_FromTo move) current_line' (α,β)
			modify' $ \ s -> s { nodesProcessed = nodesProcessed s + 1 }

			((this_val,this_subline),sub_killer_moves) <- case depth' < maxdepth of
				True -> do_search maxdepth depth' position' current_line' (α,β) killermoves
				False -> do
					modify' $ \ s -> s {
						leavesProcessed = leavesProcessed s + 1,
						nodesProcessed  = nodesProcessed  s + 1,
						evaluationsDone = evaluationsDone s + 1 }
					last_output_secs <- gets lastStateOutputTime
					TOD current_secs _ <- liftIO $ getClockTime
					when (current_secs - last_output_secs >=1) $ do
						s <- get
						liftIO $ putStrConsoleLn $ printf "[%3.0f%%]  Cutoffs:%i/%i  KillerMoveHits:%i  Best line: %+.2f  <-  %s"
							(100.0 * comp_progress [] (reverse $ computationProgress s))
							(αCutoffs s) (βCutoffs s)
							(killerMoveHits s)
							(bestVal s) (showLine (bestLine s))
--						liftIO $ putStrConsoleLn $ show $ map (length.snd) (IntMap.assocs killermoves)
						modify' $ \ s -> s { lastStateOutputTime = current_secs }
					return $ ((evalPosition position',[]),IntMap.empty)			

			let killermoves' = IntMap.unionWith uniteKillerMoves sub_killer_moves killermoves

			best'@(best_val',_) <- case this_val `isBetterThan` best_val of
				True -> do
					modify' $ \ s -> s {
						bestLine = current_line' ++ this_subline,
						bestVal = this_val,
						bestLineUpdates = bestLineUpdates s + 1 }
					return (this_val,move:this_subline)
				False -> return best

			debug_here depth' (printf "AFTER COMPARISON: BEST was %+.2f, THIS is %+.2f" best_val this_val) current_line' (α,β)

			let (α',β') = case positionColourToMove position of
				White -> ( accum_fun best_val α, β                    )
				Black -> ( α                   , accum_fun best_val β )

			modify' $ \ s -> s { computationProgress = let ((i,n):ps) = computationProgress s in (i+1,n):ps }

			case β' <= α' of
				False -> case moves of
					[] -> do
						debug_here depth ("find_best_line [] returned " ++ show best) current_line (α,β)
						return (best,killermoves')
					_  -> find_best_line best' moves (α',β') killermoves'
				True -> do
					case positionColourToMove position of
						White -> do
							debug_here depth' "beta CUTOFF: " current_line' (α,β)
							modify' $ \ s -> s { βCutoffs = βCutoffs s + 1 }
						Black -> do
							debug_here depth' "alpha CUTOFF: " current_line' (α,β)
							modify' $ \ s -> s { αCutoffs = αCutoffs s + 1 }
					killermoves'' <- case IntMap.lookup depth killermoves of
						Just kms | move `elem` kms -> do
							modify' $ \ s -> s { killerMoveHits = killerMoveHits s + 1 }
							return killermoves'
						_ -> return $ IntMap.insertWith uniteKillerMoves depth [move] killermoves'
					return (best,killermoves'')
