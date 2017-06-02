{-# LANGUAGE UnicodeSyntax,RecordWildCards,TypeSynonymInstances,FlexibleInstances,OverlappingInstances,TupleSections,DeriveGeneric #-}

module Main where

import GHC.Generics (Generic)
import Data.Array
import Data.Maybe
import Data.Ix
import Data.Ord
import Data.Char
import Data.List
import qualified Data.IntMap.Strict as Map
--import Data.Tuple
--import Data.NumInstances
import System.Random
import System.Directory
import System.IO
import Control.Monad.State.Strict
import Text.Printf
import System.CPUTime
import System.Time
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap (insert,HashMap,empty,lookup,size)

logging = False

data Piece = Ù | Ú | Û | Ü | Ý | Þ deriving (Show,Eq,Enum,Ord,Generic)
instance Hashable Piece
data Colour = White | Black deriving (Show,Eq,Enum,Ord,Read,Generic)
instance Hashable Colour
nextColour White = Black
nextColour Black = White
data File = A | B | C | D | E | F | G | H deriving (Show,Eq,Enum,Generic,Ord,Ix)
instance Hashable File
type Coors = (File,Int)

type Board = Array Coors (Maybe (Colour,Piece))
instance Hashable Board where
	hashWithSalt salt board = hashWithSalt salt (assocs board)

data Position = Position {
	pBoard              :: Board,
	pColourToMove       :: Colour,
	pCanCastleQueenSide :: [Colour],
	pCanCastleKingSide  :: [Colour],
	pEnPassantSquare    :: Maybe Coors,
	pHalfmoveClock      :: Int,
	pMoveCounter        :: Int }
	deriving (Eq,Generic)
instance Hashable Position

testPosition = Position {
	pBoard = boardFromString $
		"âçáòäçàñ" ++
		"çßîßçßîß" ++
		"ßçïðØçØç" ++
		"çØçØîØçØ" ++
		"ØçÛçÙçØç" ++
		"çØçØçÝçØ" ++
		"ÙèÙèØèÙè" ++
		"ëÚêÝíÛéÜ",
	pColourToMove       = Black,
	pCanCastleQueenSide = [],
	pCanCastleKingSide  = [],
	pEnPassantSquare    = Nothing,
	pHalfmoveClock      = 0,
	pMoveCounter        = 0 }

initialPosition = Position {
	pBoard = boardFromString $
		"âïáòäðàñ" ++
		"îßîßîßîß" ++
		"ØçØçØçØç" ++
		"çØçØçØçØ" ++
		"ØçØçØçØç" ++
		"çØçØçØçØ" ++
		"ÙèÙèÙèÙè" ++
		"ëÚêÝíÛéÜ",
	pColourToMove       = White,
	pCanCastleQueenSide = [White,Black],
	pCanCastleKingSide  = [White,Black],
	pEnPassantSquare    = Nothing,
	pHalfmoveClock      = 0,
	pMoveCounter        = 0 }

boardFromString s = array ((A,1),(H,8)) $ zip [ (f,r) | r <- [8,7..1], f <- [A .. H] ] (map to_fig s)
	where
	to_fig c_rep = case c_rep of
		c | c `elem` "ÙÚÛÜÝÞ" -> Just (White,toEnum (ord c - ord 'Ù'))
		c | c `elem` "ßàáâãä" -> Just (Black,toEnum (ord c - ord 'ß'))
		c | c `elem` "èéêëìí" -> Just (White,toEnum (ord c - ord 'è'))
		c | c `elem` "îïðñòó" -> Just (Black,toEnum (ord c - ord 'î'))
		_ -> Nothing

instance Show Position where
	show Position{..} = unlines $
		[ "¿ÀÀÀÀÀÀÀÀÁ" ] ++
		map show_rank [8,7..1] ++
		[ "ÄÏÐÑÒÓÔÕÖÆ" ] ++
		[ show pColourToMove ++ " to move" ]
		where
		show_rank rank = [ "ÇÈÉÊËÌÍÎ" !! (rank-1) ] ++ map (show_square rank) [A .. H] ++ "Ã"
		show_square rank file = case pBoard!(file,rank) of
				Nothing            | darksquare -> 'ç'
				Nothing                         -> 'Ø'
				Just (White,piece) | darksquare -> "èéêëìí" !! (fromEnum piece)
				Just (White,piece)              -> "ÙÚÛÜÝÞ" !! (fromEnum piece)
				Just (Black,piece) | darksquare -> "îïðñòó" !! (fromEnum piece)
				Just (Black,piece)              -> "ßàáâãä" !! (fromEnum piece)
			where
			darksquare = mod (rank + fromEnum file) 2 == 1

infixl 6 +++
(+++) :: Maybe Coors -> (Int,Int) -> Maybe Coors
Nothing +++ _ = Nothing
(Just (file,rank)) +++ (δfile,δrank) = case (fromEnum file + δfile, rank + δrank) of
	(i_file',rank') | i_file' `elem` [0..7] && rank' `elem` [1..8] -> Just (toEnum i_file',rank')
	_ -> Nothing

instance Show Coors where
	show (file,rank) = map toLower (show file) ++ show rank

data Move = Move {
	moveFrom :: Coors, moveTo :: Coors, moveTakes :: Maybe Coors, movePromote :: Maybe Piece }
	deriving (Eq,Ord)
instance Show Move where
	show Move{..} = show moveFrom ++ (maybe "" (const "x") moveTakes) ++ show moveTo ++ case movePromote of
		Nothing -> ""
		Just Ú -> "N"
		Just Û -> "B"
		Just Ü -> "R"
		Just Ý -> "Q"

moveIsCastling Position{..} Move{..} = case (pBoard!moveFrom,moveFrom,moveTo) of
	(Just (col,Þ),(E,_),(G,_)) -> Just (Right col)
	(Just (col,Þ),(E,_),(C,_)) -> Just (Left col)
	_ -> Nothing

moveGen pos = filter (legal_move_wrt_check pos) $ moveTargets pos

-- is colour's move ignoring check?
legal_move_wrt_check pos move = all (coors_not_in_check pos' (pColourToMove pos)) $ case moveIsCastling pos move of
	Just (Left White) ->  [(E,1),(D,1),(C,1)]
	Just (Right White) -> [(E,1),(F,1),(G,1)]
	Just (Left Black) ->  [(E,8),(D,8),(C,8)]
	Just (Right Black) -> [(E,8),(F,8),(G,8)]
	Nothing -> [ kings_coors pos' (pColourToMove pos) ]
	where
	pos' = doMove pos move

-- Checks is the player to move is in check
no_check pos = coors_not_in_check pos colour (kings_coors pos colour) where
	colour = pColourToMove pos

-- Coors of colour's king
kings_coors pos colour = head [ coors | (coors,Just (col,Þ)) <- assocs (pBoard pos), col == colour ]

-- Checks if colour's coors are threatened by the other player
coors_not_in_check pos colour coors = all ((/=coors).moveTo) moves
	where
	board = pBoard pos // [ (coors,Just (colour,Þ)) ] -- Place a figure on the square to also get pawn takes
	moves = moveTargets $ pos { pColourToMove = nextColour colour, pBoard = board }

moveTargets pos@Position{..} = concatMap piece_moves (assocs pBoard)
	where
	(north,south,east,west) = ((0,1),(0,-1),(1,0),(-1,0))
	diagonal = [ north+east,north+west,south+east,south+west ]
	straight = [ north,west,south,east ]
	knight's_moves = [ north+2*east,north+2*west,2*north+west,2*north+east,south+2*west,south+2*east,2*south+west,2*south+east ]
	piece_moves (from,Just (col,piece)) | col == pColourToMove = case piece of
		Ù -> maybe_move from ((Just from) +++ pawn_dir) ++
			(case (Just from) +++ pawn_dir of
				Just to -> case pBoard!to of
					Nothing | snd from == pawn_row -> maybe_move from ((Just from) +++ 2*pawn_dir)
					_ -> []) ++
			concat [ maybe_take from ((Just from) +++ (pawn_dir+eastwest)) | eastwest <- [east,west] ]
		Ú -> concatMap (try_move_to from) knight's_moves
		Û -> concatMap (rec_move 1 from) diagonal
		Ü -> concatMap (rec_move 1 from) straight
		Ý -> concatMap (rec_move 1 from) (diagonal++straight)
		Þ -> concatMap (try_move_to from) (diagonal++straight) ++
			if
				pColourToMove `elem` pCanCastleKingSide &&
				isEmpty (F,base_row) && isEmpty (G,base_row) then
				[ Move (E,base_row) (G,base_row) Nothing Nothing ] else []
			++
			if pColourToMove `elem` pCanCastleQueenSide &&
				isEmpty (D,base_row) && isEmpty (C,base_row) && isEmpty (B,base_row) then
				[ Move (E,base_row) (C,base_row) Nothing Nothing ] else []
	piece_moves _ = []

	pawn_dir = if pColourToMove == White then north else south

	rec_move :: Int -> Coors -> (Int,Int) -> [Move]
	rec_move i from (f,r) = case try_move_to from (f*i,r*i) of
		moves@(Move _ to Nothing _ : _) -> moves ++ rec_move (i+1) from (f,r)
		moves -> moves

	try_move_to :: Coors -> (Int,Int) -> [Move]
	try_move_to from δ = maybe_move from ((Just from) +++ δ) ++ maybe_take from ((Just from) +++ δ)

	maybe_take from (Just to) = case pBoard!to of
		Just (col,_) | col /= pColourToMove   -> [ Move from to (Just to) promo | promo <- promotions from to ]
		Nothing | pEnPassantSquare == Just to -> [ Move from to ep_pawn Nothing ] where
			ep_pawn = pEnPassantSquare +++ ( if pColourToMove==White then south else north )
		_ -> []
	maybe_take _ _ = []

	maybe_move from (Just to) = case pBoard!to of
		Nothing -> [ Move from to Nothing promo | promo <- promotions from to ]
		_ -> []
	maybe_move _ _ = []

	promotions from to = case (pBoard!from,to) of
		(Just (_,Ù),(_,8)) | pColourToMove==White -> map Just [Ú,Û,Ü,Ý]
		(Just (_,Ù),(_,1)) | pColourToMove==Black -> map Just [Ú,Û,Ü,Ý]
		_ -> [ Nothing ]

	(base_row,pawn_row) = if pColourToMove == White then (1,2) else (8,7)
	isEmpty coors = isNothing (pBoard!coors)
	
doMove pos@Position{..} mov@Move{..} = Position {
	pBoard				= pBoard // ( mb_take ++ move moveFrom moveTo ++ mb_castling ),
	pColourToMove       = nextColour pColourToMove,
	pCanCastleQueenSide = (if no_castling_queenside_any_more then delete pColourToMove else id) pCanCastleQueenSide,
	pCanCastleKingSide  = (if no_castling_kingside_any_more  then delete pColourToMove else id) pCanCastleKingSide,
	pEnPassantSquare    = case (moved_piece,moveFrom,moveTo) of
		(Ù,(file,2),(_,4)) -> Just (file,3)
		(Ù,(file,7),(_,5)) -> Just (file,6)
		_ -> Nothing,
	pHalfmoveClock      = if isJust moveTakes || moved_piece==Ù then 0 else pHalfmoveClock+1,
	pMoveCounter        = pMoveCounter + 1 }

	where

	move from to = [ (from,Nothing), (to,target_piece) ] where
		target_piece = case movePromote of
			Nothing    -> pBoard!from
			Just piece -> Just (pColourToMove,piece)

	mb_take = case moveTakes of
		Nothing    -> []
		Just coors -> [ (coors,Nothing) ]

	Just (_,moved_piece) = pBoard!moveFrom

	mb_castling = case (moved_piece,moveFrom,moveTo) of
		(Þ,(E,rank),(G,_)) -> move (H,rank) (F,rank)
		(Þ,(E,rank),(C,_)) -> move (A,rank) (D,rank)
		_                   -> []

	(no_castling_queenside_any_more,no_castling_kingside_any_more) =
		case (moveFrom,pColourToMove) of
			_ | moved_piece==Þ -> (True, True )
			((H,1),White)       -> (False,True )
			((A,1),White)       -> (True ,False)
			((H,8),Black)       -> (False,True )
			((A,8),Black)       -> (True ,False)
			_                   -> (False,False)

type Rating = Float
wHITE_MATE = -10000.0 :: Float
bLACK_MATE =  10000.0 :: Float
dRAW       =      0.0 :: Float
sTALEMATE  = dRAW

data Reason = Fifty_Halfmoves | Stalemate | NoWinPossible | Agreed deriving (Eq,Show,Ord)
data MatchResult = Mate_Winner Colour | Draw Reason deriving (Eq,Show,Ord)

rate :: Position -> (Rating,Maybe MatchResult)
rate Position{..} | pHalfmoveClock >= 50 = (dRAW,Just $ Draw Fifty_Halfmoves)
rate pos | null (moveGen pos) = case no_check pos of
	False -> case pColourToMove pos of
		White -> (wHITE_MATE,Just $ Mate_Winner Black)
		Black -> (bLACK_MATE,Just $ Mate_Winner White)
	True -> (sTALEMATE,Just $ Draw Stalemate)
rate pos | max_one_light_figure pos = (dRAW,Just $ Draw NoWinPossible)
rate pos = (0.1*mobility + sum [ (if colour==White then id else negate) piece_val |
	(coors@(file,_),Just (colour,piece)) <- assocs (pBoard pos),
	let piece_val = case piece of
		Ù -> 1 + case distance coors (file,if colour==White then 8 else 1) of
			1 -> 4
			2 -> 2
			_ -> 0
		Ú -> 3
		Û -> 3
		Ü -> 5
		Ý -> 9
		Þ -> 0 ],Nothing)
	where
	distance (file1,rank1) (file2,rank2) =
		max (abs (fromEnum file1 - fromEnum file2)) (abs (fromEnum rank1 - fromEnum rank2))
	mobility :: Rating
	mobility = fromIntegral $
		length (moveTargets $ pos { pColourToMove = White }) -
		length (moveTargets $ pos { pColourToMove = Black })

max_one_light_figure Position{..} = case sort $ filter ((/=Þ).snd) $ catMaybes $ elems pBoard of
	[]                                                                    -> True
	[(_,fig)]                 | light_figures [fig]                       -> True
	[(col1,fig1),(col2,fig2)] | light_figures [fig1,fig2] && col1 /= col2 -> True
	_                                                                     -> False
	where
	light_figures = all (`elem` [Ú,Û])

data SearchState = SearchState {
--	bestRating          :: Rating,
--	bestLine            :: [Move],
	killerMoves         :: Map.IntMap [(Rating,Move)],
	killerMoveHits      :: Int,
	memoizationHits     :: Int,
	memoizationMisses   :: Int,
	positionHashtable   :: HashMap.HashMap Position (Rating,Line),
	αCutoffs            :: Int,
	βCutoffs            :: Int,
	nodesProcessed      :: Int,
	leavesEvaluated     :: Int,
	searchStartTime     :: Integer,
	lastStateOutputTime :: Integer }
	deriving (Show)

numKillerMoves = 10

type SearchM a = StateT SearchState IO a

startSearch depth pos = do
	TOD searchstarttime _ <- liftIO getClockTime
	runStateT (searchM pos (0.0,1.0) depth [] (wHITE_MATE,bLACK_MATE)) $
		SearchState Map.empty 0 0 0 HashMap.empty 0 0 0 0 searchstarttime 0

printSearchStats SearchState{..} = liftIO $ do
--	showLine "Best line" bestRating bestLine
	putStrLn $ printf "alpha-/beta cutoffs: %i/%i" αCutoffs βCutoffs
	putStrLn $ printf "Killer move hits: %i" killerMoveHits
	putStrLn $ printf "Killer moves: %s" (show $ Map.assocs killerMoves)
	putStrLn $ printf "Hashed Positions: %i" (HashMap.size positionHashtable)
	putStrLn $ printf "Position Hashtable hits/misses: %i/%i" memoizationHits memoizationMisses
	TOD current_secs _ <- liftIO getClockTime
	let duration = current_secs - searchStartTime
	putStrLn $ printf "Search time: %is, total %i nodes/s" duration (div nodesProcessed (max 1 (fromIntegral duration))) 
	putStrLn $ printf "%i nodes processed, %i leaves evaluated" nodesProcessed leavesEvaluated

type Line = [Move]
type Depth = Int

-- α is the best value that the Maximizing player has for sure from previous alternatives
-- β is the best value that the Minimizing player has for sure from previous alternatives

showLine linestr rating line = do
	liftIO $ putStrLn $ linestr ++ " (" ++ (printf "%.2f" rating) ++ ") : " ++ show line

searchM :: Position -> (Float,Float) -> Depth -> Line -> (Rating,Rating) -> SearchM (Maybe (Rating,Line))
searchM pos@Position{..} (progress_0,progress_width) rest_depth current_line (α,β) = do
	modify $ \ s -> s { nodesProcessed = nodesProcessed s + 1 }
	let (rating,mb_matchresult) = rate pos
	ss <- get
	TOD current_secs _ <- liftIO getClockTime
	when (current_secs - lastStateOutputTime ss >= 1) $ do
		modify $ \ s -> s { lastStateOutputTime = current_secs }
		liftIO $ print pos
		liftIO $ printf "Progress: %.0f%%\n" (progress_0*100.0)
		showLine "Current line" rating current_line
		liftIO $ printf "(alpha,beta) = (%.2f,%.2f)\n" α β
		printSearchStats ss
		liftIO $ putStrLn "-------------------------------------------------------"
--		liftIO $ getLine
		return ()
	case mb_matchresult of
		Just _ -> do
			modify $ \ s -> s { leavesEvaluated = leavesEvaluated s + 1 }
			return $ Just (rating,current_line)
		Nothing -> case rest_depth of
			0 -> do
				modify $ \ s -> s { leavesEvaluated = leavesEvaluated s + 1 }
				return $ Just (rating,current_line)
			_ -> do
				posTable <- gets positionHashtable
				case HashMap.lookup pos posTable of
					Just (sub_rating,sub_line) -> do
						modify $ \ s -> s { memoizationHits = memoizationHits s + 1 }
						return $ Just (sub_rating,sub_line++current_line)
					Nothing -> do
						modify $ \ s -> s { memoizationMisses = memoizationMisses s + 1 }
						SearchState{..} <- get
						let
							killer_moves = map snd $ Map.findWithDefault [] rest_depth killerMoves
							sorted_moves = (gen_moves `intersect` killer_moves) ++ (gen_moves \\ killer_moves)
						try_moves sorted_moves (if maximizer then wHITE_MATE else bLACK_MATE, []) --(if maximizer then α else β, [])
						where
						gen_moves = moveGen pos
						num_moves = fromIntegral $ length gen_moves
						try_moves [] result = do
							when logging $ do
								liftIO $ withFile "log.txt" AppendMode $ \ h -> do
									hPutStrLn h $ printf "%s= %s" (indent $ length current_line) (show result)
							return $ Just result
						try_moves (move:moves) (best_rating,best_line) = do
							let progress = (progress_0+progress_width*(num_moves - (fromIntegral $ length moves + 1)) / num_moves,
								progress_width/num_moves)
							liftIO $ withFile "log.txt" AppendMode $ \ h -> do
								hPutStrLn h $ printf "%s%s" (indent $ length current_line) (show move)
							mb_sub <- searchM (doMove pos move) progress (rest_depth-1) (move:current_line) $
								if maximizer then (best_rating,β) else (α,best_rating)
							case mb_sub of
								Nothing -> try_moves moves (best_rating,best_line)
								Just (sub_rating,sub_line) -> do
									when logging $ do
										liftIO $ withFile "log.txt" AppendMode $ \ h -> do
											hPutStrLn h $ printf "%s-> %.2f" (indent $ length current_line) sub_rating
									modify $ \ s -> s { positionHashtable = HashMap.insert pos (sub_rating,sub_line) (positionHashtable s) }
									case (if maximizer then (>=) else (<=)) sub_rating best_rating of
										False -> try_moves moves (best_rating,best_line)
										True  -> case if maximizer then sub_rating >= β else sub_rating <= α of
											True -> do  -- CUTOFF
												liftIO $ withFile "log.txt" AppendMode $ \ h -> do
													hPutStrLn h $ printf "%sCUTOFF: sub_rating=%.2f, alpha=%.2f, beta=%.2f" (indent $ length current_line) sub_rating α β
												killermoves <- gets killerMoves
												when (move `elem` (map snd (Map.findWithDefault [] rest_depth killermoves))) $ do
													modify $ \ s -> s { killerMoveHits = killerMoveHits s + 1 }
												modify $ \ s -> s { killerMoves = Map.alter (insert_killer_move sub_rating move) rest_depth (killerMoves s) }
												modify $ \ s -> case maximizer of
													True  -> s { βCutoffs = βCutoffs s + 1 }
													False -> s { αCutoffs = αCutoffs s + 1 }
												return Nothing
											False -> try_moves moves (sub_rating,sub_line)
	where
	maximizer = pColourToMove == White
	insert_killer_move rating move Nothing = Just [(rating,move)]
	insert_killer_move rating move (Just killermoves) = Just $ take numKillerMoves $ nub $
		(if maximizer then reverse else id) $ sort $ (rating,move):killermoves

indent n = take (4*n) $ cycle "|   "

main = do
	loop 4 [initialPosition]

loop depth poss@(pos:lastpos) = do
	case rate pos of
		(rating,mb_matchresult) -> do
			print pos
			case mb_matchresult of
				Just matchresult -> print matchresult
				Nothing -> do
					print moves
					get_input
					where
					moves = moveGen pos
					get_input = do
						putStr "? "
						s <- getLine
						case s of
							"i" -> loop depth [initialPosition]
							colourstring | [(colour,"")] <- reads colourstring -> loop depth (pos { pColourToMove = colour } : poss)
							"t" -> loop depth [testPosition]
							"random" -> randomMatch pos
							"q" -> return ()
							"s" -> do
								when logging $ liftIO $ removeFile "log.txt"
								(Just (rating,moves),ss) <- startSearch depth pos
								putStrLn $ "BEST LINE (" ++ show rating ++ "): " ++ show moves
								printSearchStats ss
								loop depth (doMove pos (last moves) : poss)
							"r" -> do
								putStrLn $ "Rating: " ++ show (rate pos)
								loop depth poss
							"b" -> loop depth lastpos
							depthstr | all isDigit depthstr -> do
								let [(depth,"")] = reads depthstr
								putStrLn $ "Set depth to " ++ show depth
								loop depth poss
							m -> case filter ((m==).snd) (zip moves (map show moves)) of
								[] -> do
									putStrLn "No move."
									get_input
								[(move,_)] -> loop depth (doMove pos move : poss)

					randomMatch pos = case rate pos of
						(_,Just ending) -> print ending
						(rating,Nothing) -> do
							putStrLn $ "Rating=" ++ show rating
							let moves = (if pColourToMove pos == White then reverse else id) $
								sortBy (comparing fst) $ map (\ m -> (rate $ doMove pos m,m)) $ moveGen pos
							r <- randomIO
							let (_,move) = moves!!(mod r $ min 5 (length moves))
							putStrLn $ "Moving " ++ show move
							let pos' = doMove pos move
							print pos'
							randomMatch pos'
		