--We will use unicode symbols in the code and some language extensions...

{-# LANGUAGE UnicodeSyntax,RecordWildCards,TypeSynonymInstances,FlexibleInstances,
	OverlappingInstances,TupleSections,StandaloneDeriving,DeriveGeneric #-}

module Main where

import GHC.Generics (Generic)
import qualified Data.Set as Set
import Data.Char
import Data.Array
import Data.Maybe
import Data.List
import Data.Stack
import Data.NumInstances
import Data.Ord
import Text.Printf
import Control.Parallel.Strategies
import System.IO
import System.CPUTime
import System.Time
import qualified Data.IntMap.Strict as Map
import Control.Monad.State.Strict
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap (insert,HashMap,empty,lookup,size)

--In chess, two players

data Colour = White | Black
	deriving (Show,Eq,Enum,Bounded,Ord,Generic)
instance Hashable Colour

--are taking turns

nextColour White = Black
nextColour Black = White

--in moving pieces

data Piece = Ù | Ú | Û | Ü | Ý | Þ
	deriving (Eq,Enum,Bounded,Ord,Show,Generic)
instance Hashable Piece

--on a board, which is an array of squares indexed by coordinates:

type Board = Array Coors Square
instance Hashable Board where
	hashWithSalt salt board = hashWithSalt salt (assocs board)

--On a square, maybe there is a piece of a colour (or nothing).

type Square = Maybe (Colour,Piece)

--In chess, the file coordinate is a letter

data File = A | B | C | D | E | F | G | H
	deriving (Show,Eq,Ix,Ord,Enum,Generic)
instance Hashable File

--and the rank is an integer number.
--Hence, the coordinates are the (cartesian) product

type Coors = (File,Int)
instance Show Coors where
	show (file,rank) = map toLower (show file) ++ show rank

--A position in chess consists of
--- the current board,
--- the colour to move,
--- the lists of players that still have the right to castle queen side or king side,
--- whether a pawn could be taken en passant (double pawn step before),
--- and a clock counting the half moves that have been made
--  (chess rules say that the game is drawn if for 50+ half moves, no pawn is moved or piece is taken).
--- Last but not least, we keep track of the next move's number.

data Position = Position {
	pBoard              :: Board,
	pColourToMove       :: Colour,
	pCanCastleQueenSide :: [Colour],
	pCanCastleKingSide  :: [Colour],
	pEnPassant          :: Maybe (Coors,Coors),
	pHalfmoveClock      :: Int,
	pNextMoveNumber     :: Int }
	deriving (Eq,Generic)
instance Hashable Position

--In the initial position, White is to start,
--with both players having all castling rights.
--There is no pawn that could be taken en passant,
--and the half move clock starts at zero:

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
	pCanCastleQueenSide = allOfThem,
	pCanCastleKingSide  = allOfThem,
	pEnPassant          = Nothing,
	pHalfmoveClock      = 0,
	pNextMoveNumber     = 1 }

--The allOfThem function generates a list of all values of the respective type,
--which is needed above.

allOfThem :: (Enum a,Bounded a,Ord a) => [a]
allOfThem = [minBound..maxBound]

--Castling and promotion happen on base ranks:

baseRank White = 1
baseRank Black = 8

--We play on a cartesian board in two dimensions with the basis {north,east}:

(north,east) = ((0,1),(1,0))
(south,west) = (-north,-east)

--White's pawns move northwards, Black's pawns southwards.

pawnStep White = north
pawnStep Black = south

--In order to print a board, we define a show_square function:

show_square darksquare square = case square of 
	Nothing            | darksquare -> 'ç'
	Nothing                         -> 'Ø'
	Just (White,piece) | darksquare -> "èéêëìí" !! (fromEnum piece)
	Just (White,piece)              -> "ÙÚÛÜÝÞ" !! (fromEnum piece)
	Just (Black,piece) | darksquare -> "îïðñòó" !! (fromEnum piece)
	Just (Black,piece)              -> "ßàáâãä" !! (fromEnum piece)

--read_square is (almost) the inverse of show_square, we don't have to give both directions
--and have single source at the same time...

read_square c = lookup c [ (show_square dark (Just (col,piece)), (col,piece)) |
	col <- allOfThem, piece <- allOfThem, dark <- allOfThem ]

--The boardFromString convenience function converts a string to
--a board with pieces on it, starting starting from (A,8) in the upper left corner. 

boardFromString s = array ((A,1),(H,8)) $ zip [ (f,r) | r <- [8,7..1], f <- [A .. H] ] (map read_square s)

--In order to print a chess position, we make Position an instance of Show:

instance Show Position where
	show Position{..} = printf "¿ÀÀÀÀÀÀÀÀÁ\n%sÄÏÐÑÒÓÔÕÖÆ\n%s to do move %i\n"
		(unlines $ map show_rank [8,7..1]) (show pColourToMove) pNextMoveNumber
		where
		show_rank rank = [ "ÇÈÉÊËÌÍÎ" !! (fromEnum rank - 1) ] ++
			[ show_square (is_darksquare (file,rank)) (pBoard!(file,rank)) | file <- [A .. H] ] ++ "Ã"
			where
			is_darksquare (file,rank) = mod (fromEnum rank + fromEnum file) 2 == 1

--In order to calculate target square coordinates, we need to add deltas to coordinates.
--Therefore we define a new infix operator "+++", which is left associative and has precedence 6
--(same as the normal "+" operator). 

infixl 6 +++

--The addition result maybe out of the board's bounds, hence the result type in

(+++) :: Coors -> (Int,Int) -> Maybe Coors
(file,rank) +++ (δfile,δrank) | ifile' `elem` [0..7] && rank' `elem` [1..8] =
	Just (toEnum ifile',rank') where
	(ifile',rank') = (fromEnum file + δfile, rank + δrank)
_ +++ _ = Nothing

--A move is from a coordinate to another coordinate,
--might take an opponent's piece
--(for en passant, from another square than the target),
--and might also promote a pawn to another piece.

data Move =
	Move {
		moveFrom    :: Coors,
		moveTo      :: Coors,
		moveTakes   :: Maybe Coors,
		movePromote :: Maybe Piece } |
	Castling CastlingSide
	deriving Eq
data CastlingSide = Queenside | Kingside deriving Eq

instance Show Move where
	show Move{..} = show moveFrom ++ show moveTo ++ case movePromote of
		Nothing -> ""
		Just Ú -> "N"
		Just Û -> "B"
		Just Ü -> "R"
		Just Ý -> "Q"
	show (Castling Queenside) = "O-O-O"
	show (Castling Kingside)  = "O-O"

doMove pos@Position{..} move = pos' {
	pCanCastleQueenSide = if disabled_queenside then pColourToMove `delete` pCanCastleQueenSide else pCanCastleQueenSide,
	pCanCastleKingSide  = if disabled_kingside  then pColourToMove `delete` pCanCastleKingSide  else pCanCastleKingSide,
	pColourToMove       = nextColour pColourToMove,

--The halfmove clock increases with every consecutive move other than a pawn's and without
--taking a piece. After 50 such "half"moves the match is drawn.

	pHalfmoveClock = case move of
		Move { moveTakes = Just _ }                -> 0
		Move {..} | Just (_,Ù) <- pBoard!moveFrom -> 0
		_ | otherwise                              -> pHalfmoveClock + 1,

	pNextMoveNumber = if pColourToMove==Black then pNextMoveNumber+1 else pNextMoveNumber,

--If a pawn advances a double step, enable possibility of taking it "en passant" in the
--next move by saving the pawn's intermediate and target square.

	pEnPassant = case move of
		Move from to Nothing Nothing |
			Just (_,Ù) <- pBoard!from,
			Just to == from +++ 2*pawn_step,
			Just middle <- from +++ pawn_step -> Just (middle,to)
		_ | otherwise                        -> Nothing }
	where
	pawn_step = pawnStep pColourToMove 
	(disabled_queenside,disabled_kingside) = case (pColourToMove,move) of
		(_,    Castling _      ) -> (True, True )
		(White,Move (E,1) _ _ _) -> (True, True )
		(Black,Move (E,8) _ _ _) -> (True, True )
		(White,Move (A,1) _ _ _) -> (True, False)
		(Black,Move (A,8) _ _ _) -> (True ,False)
		(White,Move (H,1) _ _ _) -> (False,True )
		(Black,Move (H,8) _ _ _) -> (False,True )
		_ | otherwise            -> (False,False)
	pos' = pos { pBoard = pBoard // case move of
		Move{..} -> maybe [] (\ take_coors -> [(take_coors,Nothing)]) moveTakes ++
			[ (moveFrom,Nothing), (moveTo,maybe (pBoard!moveFrom) (Just.(pColourToMove,)) movePromote) ]
		Castling Queenside -> [ ((A,r),Nothing), ((D,r),pBoard!(A,r)), ((E,r),Nothing), ((C,r),pBoard!(E,r)) ]
		Castling Kingside  -> [ ((H,r),Nothing), ((F,r),pBoard!(H,r)), ((E,r),Nothing), ((G,r),pBoard!(E,r)) ] } where
		r = baseRank pColourToMove

--In a chess match, either one colour checkmates the other or it is a draw for some reason
--(one could also resign, of course...)

data MatchResult = Winner Colour WinReason | Draw DrawReason deriving Show
data WinReason  = Resignation | Checkmate deriving Show
data DrawReason = Fifty_Halfmoves | Stalemate | NoWinPossible deriving Show

--The move generator generates all legal moves in a position by first calculating
--all potential moves and then filtering out the moves that are not allowed
--because the king would be in check.

moveGen pos@Position{..} = filter king_not_in_check $ potentialMoves pos
	where
	king_not_in_check move = all (coorsNotInCheck pos_after_move pColourToMove) $ case move of
		Castling Queenside -> [(E,r),(D,r),(C,r)]
		Castling Kingside  -> [(E,r),(F,r),(G,r)]
		Move{..}           -> [ kingsCoors pos_after_move pColourToMove ]
		where
		r = baseRank pColourToMove
		pos_after_move = doMove pos move

--Is a square threatened by a piece of a given colour?

coorsNotInCheck pos colour coors = all (/=coors) [ moveTo |
	Move{..} <- potentialMoves $ pos {
		-- Say, the next colour would be to move now...
		pColourToMove = nextColour colour,
		-- ... and place some figure at the coors to also get pawn takes as potential moves:
		pBoard = pBoard pos // [ (coors,Just (colour,Ý)) ] } ]

notInCheck pos@Position{..} = coorsNotInCheck pos pColourToMove $ kingsCoors pos pColourToMove

kingsCoors pos colour = head [ coors | (coors,Just (col,Þ)) <- assocs (pBoard pos), col == colour ]

potentialMoves pos@Position{..} = [ Move move_from move_to mb_takes mb_promote |
	(move_from@(_,from_rank),Just (colour,piece)) <- assocs pBoard, colour==pColourToMove,
	(move_to@(_,to_rank),mb_takes) <- let
		initial_rank = if pColourToMove==White then 2 else 7
		pawn_step = pawnStep pColourToMove 
		diagonals = [ north+east,north+west,south+east,south+west ]
		straights = [ north,west,south,east ]
	 	knight_moves = [ north+2*east,north+2*west,2*north+west,2*north+east,south+2*west,south+2*east,2*south+west,2*south+east ]
		maybe_move from δ = case from +++ δ of
			Nothing -> []
			Just move_to -> case pBoard!move_to of
				Nothing                             -> [ (move_to,Nothing) ]
				Just (col,_) | col /= pColourToMove -> [ (move_to,Just move_to) ]
				_            | otherwise            -> []
		maybe_move_direction from δ = case maybe_move from δ of
			move@[ (_,Just _)   ] -> move
			move@[ (to,Nothing) ] -> move ++ maybe_move_direction to δ
			_ | otherwise         -> []
		in case piece of
			Ù -> ( case move_from +++ pawn_step of

--If the square in front of the pawn direction is empty, it could move there:

				Just move_to | Nothing <- pBoard!move_to -> [ (move_to,Nothing) ] ++

--If, moreover, the pawn still stands on its initial position, it could even move two squares
--given that this target square is also empty:

					case move_from +++ 2*pawn_step of
						Just move_to2 | Nothing <- pBoard!move_to2, from_rank==initial_rank -> [ (move_to2,Nothing) ]
						_ -> []
				_            | otherwise -> [] ) ++

--A pawn can take pieces diagonally in front of it,
--even intercepting a two square move of an opponent's pawn ("en passant"):

				[ (move_to,Just take_on) | Just move_to <- map (move_from +++) [pawn_step+east,pawn_step+west],
					take_on <- case pBoard!move_to of

--If there is an opponent's piece on target square, one can take it:

						Just (colour,_) | colour /= pColourToMove                                   -> [ move_to ]

--If en passant is enabled with the middle square being the current move's target, one can also take:

						_               | Just (middle,pawn_coors) <- pEnPassant, middle == move_to -> [ pawn_coors ]
						_               | otherwise                                                 -> [] ]

--For a knight, there is a fixed set of target squares.

			Ú -> concatMap (maybe_move           move_from) knight_moves

--A bishop can move from its location in each of the diagonals' direction:

			Û -> concatMap (maybe_move_direction move_from) diagonals

--Same for rooks, but in straight direction:

			Ü -> concatMap (maybe_move_direction move_from) straights

--The queen can move is each direction.

			Ý -> concatMap (maybe_move_direction move_from) (straights++diagonals)

--The king can only move one step, but also in each direction.

			Þ -> concatMap (maybe_move           move_from) (straights++diagonals),

--Only a pawn will be promoted to a piece once it reaches the opposite base rank:

	mb_promote <- case piece of
		Ù | to_rank == baseRank (nextColour pColourToMove) -> map Just [Ý,Ú,Û,Ü]
		_ | otherwise                                      -> [ Nothing ] ] ++

	let
		r = baseRank pColourToMove
		square_empty coors = isNothing $ pBoard!coors
	in

--A player might castle kingside or queenside, if both the king and the rook haven't moved yet
--(i.e. the player's colour is stored in pCanCastle<X>Side) and the squares between them are empty.

	[ Castling Kingside  | pColourToMove `elem` pCanCastleKingSide,  all square_empty [(F,r),(G,r)] ] ++
	[ Castling Queenside | pColourToMove `elem` pCanCastleQueenSide, all square_empty [(D,r),(C,r),(B,r)] ]

--The rating of a position is a float number with a minimum and a maximum rating.
--An equal postion's rating is 0.

type Rating = Float
mAX         = 10000.0
mIN         = negate mAX
eQUAL       =     0.0

--The rate function will return a rating of the position together
--with an match result if the match ends.

rate :: Position -> (Rating,Maybe MatchResult)

--If there is no pawn moved or piece taken for 50 half-moves,
--the game is drawn:

rate Position{..} | pHalfmoveClock >= 50 = (eQUAL,Just $ Draw Fifty_Halfmoves)

--If there is no legal move for one colour, we have either a stalemate,
--or a checkmate if the king is in check:

rate pos | moveGen pos == [] = case (notInCheck pos,pColourToMove pos) of
	(True,_)      -> (eQUAL,Just $ Draw Stalemate)
	(False,White) -> (mIN,  Just $ Winner Black Checkmate)
	(False,Black) -> (mAX,  Just $ Winner White Checkmate)

--With only one bishop or knight (so called "light figures"), one cannot checkmate:

rate pos@Position{..} | max_one_light_figure = (eQUAL,Just $ Draw NoWinPossible) where
	max_one_light_figure = case sort $ filter ((/=Þ).snd) $ catMaybes $ elems pBoard of
		[]                                                          -> True
		[(_,fig)]                   | all_light_figures [fig]       -> True
		[(White,fig1),(Black,fig2)] | all_light_figures [fig1,fig2] -> True
		_                           | otherwise                     -> False
	all_light_figures = all (`elem` [Ú,Û])

--Otherwise, we will rate the position based on material, distance of pawns from promotion,
--and the mobility of each piece.

rate pos = (rating,Nothing) where
	rating = 0.01*mobility + sum [ (if colour==White then id else negate) (piece_val piece colour coors) |
		(coors,Just (colour,piece)) <- assocs $ pBoard pos ]
	piece_val Ù colour (_,rank) = 1 + case abs (rank - baseRank (nextColour colour)) of
				1             -> 4
				2             -> 2
				_ | otherwise -> 0
	piece_val Ú _ _ = 3
	piece_val Û _ _ = 3
	piece_val Ü _ _ = 5
	piece_val Ý _ _ = 9
	piece_val Þ _ _ = 0
	mobility = fromIntegral $
		length (potentialMoves $ pos { pColourToMove = White }) -
		length (potentialMoves $ pos { pColourToMove = Black })

--The search function calculates the best move up to a certain depth according
--to the minimax algorithm.
--We represent the computing depth as an integer.

type Depth = Int
type Line = [Move]

search :: Bool -> Depth -> Position -> Line -> (Rating,Line)
search _        depth pos              line | moveGen pos == [] || depth==0 = (fst $ rate pos,line)
search parallel depth pos@Position{..} line = minimax (comparing fst) (functor deeper $ moveGen pos) where

--Since with pure functional code there can't be any race conditions or deadlocks,
--we can easily distribute the search on multicore by using a parallel map:

	functor     = if parallel then parMap rpar else map
	minimax     = if pColourToMove == White then maximumBy else minimumBy
	deeper move = search False (depth-1) (doMove pos move) (move:line)

--In order to play a match vs. the computer, we need an interaction loop accepting input
--from the console.

main = loop 6 initialPosition stackNew where
	loop :: Depth -> Position -> Stack Position -> IO ()
	loop maxdepth pos pos_history = do
		print pos
		putStrLn $ printf "Rating = %.2f" (fst $ rate pos)
		putStrLn $ "Possible moves are:" ++ show (moveGen pos)
		case rate pos of

--Note that in the rate function call above, the actual rating number won't be computed because it is not needed
--("lazy evaluation") because is only matched against wildcards below:

			(_,Just matchresult) -> print matchresult
			_ | otherwise        -> return ()
		input <- putStr "? " >> hFlush stdout >> getLine
		case input of
			"i" -> loop maxdepth initialPosition stackNew
			"q" -> return ()
			"s" -> execute_move $ last $ snd $ search False maxdepth pos []
			"p" -> execute_move $ last $ snd $ search True  maxdepth pos []
			"a" -> do
				(_,moves) <- alphabeta maxdepth pos (mIN,mAX) []
				when (not $ null moves) $ execute_move (last moves)
			"d" -> do
				(_,moves) <- iter_deep maxdepth pos
				when (not $ null moves) $ execute_move (last moves)
			"b" -> case stackPop pos_history of
				Nothing -> do
					putStrLn "There is no previous position."
					loop maxdepth pos pos_history
				Just (stack',prev_pos) -> loop maxdepth prev_pos stack'
			depth_str | [(depth,[])] <- reads depth_str -> loop depth pos pos_history
			move_str -> case lookup move_str $ map (\ m -> (show m,m)) $ moveGen pos of
				Nothing   -> do
					putStrLn "This is no (legal) move or command."
					loop maxdepth pos pos_history
				Just move -> execute_move move
		where
		execute_move move = loop maxdepth (doMove pos move) (stackPush pos_history pos)

--The alpha-beta search keeps two values α and β, representing the minimum result that both sides
--have for sure already in the current position. This leads to cutoffs for paths outside of this window.

data SearchState = SearchState {
-- 	killerMoves         :: Map.IntMap [(Rating,Move)],
	killerMoves         :: [Move],
	killerMoveHits      :: Int,
	memoizationHits     :: Int,
	memoizationMisses   :: Int,
	positionHashtable   :: HashMap.HashMap Position (Rating,Line),
	principalVariation  :: Line,
	αCutoffs            :: Int,
	βCutoffs            :: Int,
	nodesProcessed      :: Int,
	leavesEvaluated     :: Int,
	searchStartTime     :: Integer,
	lastStateOutputTime :: Integer }
	deriving (Show)

printSearchStats SearchState{..} pos (α,β) progressmin rating current_line = liftIO $ do
	putStrLn $ printf "Progress: %.0f%%\n" (progressmin*100.0)
	showLine "Current line" rating current_line
	putStrLn $ printf "(alpha,beta) = (%.2f,%.2f)\n" α β
	putStrLn $ printf "alpha-/beta cutoffs: %i/%i" αCutoffs βCutoffs
	putStrLn $ printf "Principal Variation: %s"  (show principalVariation)
	putStrLn $ printf "Killer move hits: %i" killerMoveHits
	putStrLn $ printf "Killer moves: %s" (show killerMoves)
	putStrLn $ printf "Hashed Positions: %i" (HashMap.size positionHashtable)
	putStrLn $ printf "Position Hashtable hits/misses: %i/%i" memoizationHits memoizationMisses
	TOD current_secs _ <- liftIO getClockTime
	let duration = current_secs - searchStartTime
	putStrLn $ printf "Search time: %is, total %i nodes/s" duration (div nodesProcessed (max 1 (fromIntegral duration))) 
	putStrLn $ printf "%i nodes processed, %i leaves evaluated" nodesProcessed leavesEvaluated
	putStrLn "-------------------------------------------------------"
--	getLine

numKillerMoves = 10

type SearchM a = StateT SearchState IO a

showLine linestr rating line = do
	liftIO $ putStrLn $ linestr ++ " (" ++ (printf "%.2f" rating) ++ ") : " ++ show line

alphabeta :: Depth -> Position -> (Rating,Rating) -> Line -> IO (Rating,Line)
alphabeta maxdepth pos (α,β) principal_var = do
	TOD searchstarttime _ <- liftIO getClockTime
	evalStateT (alphabetaM pos (0.0,1.0) maxdepth [] (α,β)) $
		SearchState [] 0 0 0 HashMap.empty principal_var 0 0 0 0 searchstarttime 0

alphabetaM :: Position -> (Float,Float) -> Depth -> Line -> (Rating,Rating) -> SearchM (Rating,Line)
alphabetaM pos@Position{..} (progressmin,progressmax) rest_depth current_line (α,β) = do
	modify $ \ s -> s { nodesProcessed = nodesProcessed s + 1 }
	let (rating,mb_matchresult) = rate pos
	ss <- get
	TOD current_secs _ <- liftIO getClockTime
	when (current_secs - lastStateOutputTime ss >= 1) $ do
		modify $ \ s -> s { lastStateOutputTime = current_secs }
		printSearchStats ss pos (α,β) progressmin rating current_line
	case rest_depth==0 || isJust mb_matchresult of
		True -> do
			modify $ \ s -> s { leavesEvaluated = leavesEvaluated ss + 1 }
			return (rating,current_line)
		False -> do
			case HashMap.lookup pos (positionHashtable ss) of
				Just (sub_rating,sub_line) -> do
					modify $ \ s -> s { memoizationHits = memoizationHits s + 1 }
					return (sub_rating,sub_line++current_line)
				Nothing -> do
					modify $ \ s -> s { memoizationMisses = memoizationMisses s + 1 }
					let
						lower_val_captures = [ move | move@(Move from _ (Just capture) _) <- legal_moves,
							Just (_,capturing_piece) <- [pBoard!from], Just (_,captured_piece) <- [pBoard!capture],
							capturing_piece <= captured_piece ]
						recaptures = [ move | move@(Move _ _ (Just capture) _) <- legal_moves,
							Move _ _ (Just previous_capture) _ <- take 1 current_line,
							capture == previous_capture ]
					let
						principal_moves = take 1 $ drop (rest_depth-1) (principalVariation ss)
						moves = nub $ (principal_moves ++ killerMoves ss) `intersect` legal_moves ++ lower_val_captures ++ recaptures ++ legal_moves
					result <- try_moves moves (if maximizer then α else β,[])
					modify $ \ s -> s { positionHashtable = HashMap.insert pos result (positionHashtable s) }
					return result

					where

					legal_moves = moveGen pos
					maximizer = pColourToMove==White
					isBetterThan = if maximizer then (>) else (<)
					try_moves [] result = return result
					try_moves (move:moves) (best_subrating,best_subline) = do
						let
							progressmin' = progressmin + (fromIntegral $ length legal_moves - length (move:moves))*(progressmax-progressmin) / (fromIntegral $ length legal_moves)
							progressmax' = progressmin' + (progressmax-progressmin) / (fromIntegral $ length legal_moves)
						(subrating,subline) <- alphabetaM (doMove pos move) (progressmin',progressmax') (rest_depth-1) (move:current_line) $
							if maximizer then (best_subrating,β) else (α,best_subrating)
						case subrating `isBetterThan` best_subrating of
							False -> try_moves moves (best_subrating,best_subline)
							True  -> case if maximizer then subrating >= β else subrating <= α of
								True -> do -- CUTOFF
									modify $ \ s@SearchState{..} -> s {
										killerMoveHits = if move `elem` killerMoves then killerMoveHits + 1 else killerMoveHits,
										killerMoves    = take numKillerMoves $ nub $ move : killerMoves,
										βCutoffs       = if maximizer then βCutoffs + 1 else βCutoffs,
										αCutoffs       = if maximizer then αCutoffs else αCutoffs + 1 }
									return (subrating,subline)
								False -> try_moves moves (subrating,subline)

iter_deep maxdepth pos = do
	iter_deep_loop maxdepth 2 (mIN,mAX) []
	where
	iter_deep_loop maxdepth cur_depth (α,β) principal_var = do
		putStrLn "######################################################"
		putStrLn $ printf "Current/max depth: %i/%i" cur_depth maxdepth
		putStrLn $ printf "Principal Variation: %s" (show $ reverse principal_var)
--		getLine
		(rating,line) <- alphabeta cur_depth pos (α,β) principal_var
		case line of
			[] -> do -- No line found => widen window
				let (α',β') = if rating <= α then (α-(β-α)/2,β) else (α,β+(β-α)/2)
				putStrLn $ printf "FAILED with (%.2f,%.2f), widening to (%.2f,%.2f)" α β α' β'
				iter_deep_loop maxdepth cur_depth (α',β') principal_var
			_  -> do
				putStrLn $ printf "found line (%.2f): %s" rating (show $ reverse line)
				case cur_depth < maxdepth of
					True  -> iter_deep_loop maxdepth (cur_depth+2) (rating-0.3,rating+0.3) line
					False -> return (rating,line)
