{-# LANGUAGE TupleSections,TypeSynonymInstances,FlexibleInstances,ScopedTypeVariables,RecordWildCards,FlexibleContexts,UnicodeSyntax,DeriveGeneric #-}

module Main where

import GHC.Generics (Generic)
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap (insert,HashMap,empty,lookup,size) 

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
import Text.Parsec
import Text.Parsec.String

import System.IO.Unsafe

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU

type File = Int
type Rank = Int
type Coors = (File,Rank)

type Depth = Int

data Colour = White | Black
	deriving (Eq,Show,Enum,Ord,Generic)
instance Hashable Colour

nextColour White = Black
nextColour Black = White
coloursToMove = iterate nextColour White

data PieceType = Ù | Ú | Û | Ü | Ý | Þ
	deriving (Show,Eq,Enum,Ord,Ix,Generic)
instance Hashable PieceType

type Piece = (Colour,PieceType)

type Board = Array Coors (Maybe Piece)
instance Hashable Board where
	hashWithSalt salt board = hashWithSalt salt (assocs board)

stringToPosition :: Colour -> [String] -> Position
stringToPosition col_to_move s = Position (array ((1,1),(8,8)) $
	zip [ (f,r) | r <- [8,7..1], f <- [1..8] ] (map tofig (concat s)))
	col_to_move [White,Black] [White,Black] Nothing 0 0
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

type CastlingRights = [Colour]

data Position = Position {
	positionBoard              :: Board,
	positionColourToMove       :: Colour,
	positionCanCastleQueenSide :: CastlingRights,
	positionCanCastleKingSide  :: CastlingRights,
	positionEnPassantSquare    :: Maybe Coors,
	positionHalfmoveClock      :: Int,
	positionMoveCounter        :: Int }
	deriving (Generic,Eq)
instance Hashable Position

instance Show Position where
	show Position{..} = show positionBoard

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
		(if castled_queen || castled_king || from `elem` [(5,castle_rank),(1,castle_rank)] then delete my_colour castlequeen else castlequeen)
		(if castled_queen || castled_king || from `elem` [(5,castle_rank),(8,castle_rank)] then delete my_colour castleking else castleking)
		( case (moved_piecetype,from,to) of
			(Ù,(r,f0),(_,f1)) | abs (f1-f0) == 2 -> Just (r,(div (f0+f1) 2))
			_ -> Nothing )
		(if isJust mb_take || moved_piecetype==Ù then 0 else halfmoves+1)
		(if colour==Black then movecounter+1 else movecounter)
	where
	castle_rank = castleRank my_colour
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
			if colour_to_move `elem` cancastleking then
				case map ((board!).(,castle_rank)) [5..8] of
					[ Just (kingcol,Þ),Nothing,Nothing,Just (rookcol,Ü) ] |
						kingcol==colour_to_move && rookcol==colour_to_move &&
						all (no_check position) [(5,castle_rank),(6,castle_rank)] ->
							[ Move (5,castle_rank) (7,castle_rank) Nothing Nothing ]
					_ -> []
				else []
			++
			if colour_to_move `elem` cancastlequeen then
				case map ((board!).(,castle_rank)) [1..5] of
					[ Just (rookcol,Ü),Nothing,Nothing,Nothing,Just (kingcol,Þ) ] |
						kingcol==colour_to_move && rookcol==colour_to_move &&
						all (no_check position) [(4,castle_rank),(5,castle_rank)] ->
							[ Move (5,castle_rank) (3,castle_rank) Nothing Nothing ]
					_ -> []
				else []
			of
			[] -> Left $ (if no_check position (kings_coors position) then Stalemate else Checkmate) colour_to_move
			moves -> Right $ map checkmv moves

	where

	checkmv mv = let
		pos' = doMove position mv
		in
		case [ coors | (coors,Just (col,Þ)) <- assocs (positionBoard pos'), col==positionColourToMove pos' ] of
			[] -> error $ show mv ++ " in pos " ++ (unsafePerformIO $ do
				liftIO $ showPos position
				return "")
			_ -> mv

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

fENFigureChars = [(Ù,"P"),(Ú,"N"),(Û,"B"),(Ü,"R"),(Ý,"Q"),(Þ,"K")]

toFEN :: Position -> String
toFEN (Position board colour castlequeen castleking mb_ep halfmove_clock movecounter) =
	intercalate "/" [ row2fen 0 [ board!(f,r) | f <- [1..8] ] | r <- [8,7..1] ] ++ " " ++
	(if colour==White then "w" else "b") ++ " " ++
	(if castles=="" then "-" else castles) ++ " " ++
	maybe "-" showCoors mb_ep ++ " " ++
	show halfmove_clock ++ " " ++ show movecounter
	where
	castles =
		(if White `elem` castleking  then "K" else "") ++
		(if White `elem` castlequeen then "Q" else "") ++
		(if Black `elem` castleking  then "k" else "") ++
		(if Black `elem` castlequeen then "q" else "")
	row2fen :: Int -> [Maybe (Colour,PieceType)] -> String
	row2fen 0 [] = ""
	row2fen cnt [] = show cnt
	row2fen cnt (Nothing:rs) = row2fen (cnt+1) rs
	row2fen cnt ((Just (col,piecetype)) : rs) = (if cnt>0 then show cnt else "") ++
		map (if col==White then toUpper else toLower) (fromJust $ lookup piecetype fENFigureChars) ++
		row2fen 0 rs

epd_p = do
	pos <- fenpos_p
	string " bm "
	line <- sepBy1 (move_p pos) (string " ")
	string "; id \""
	idstr <- manyTill anyChar (try $ string "\"")
	string ";"
	return (idstr,pos,line)
	where
	movestr_p (str,move) = string str >> return move
	move_p pos = choice $ map movestr_p $ zip (map (showMove pos) moves) moves
		where
		Right moves = moveGenerator pos 

testsuite_p = sepBy epd_p (spaces >> newline)
	

t = do
	f <- readFile "BK-Test.txt"
	let ls = lines f
	print $ parse epd_p "" (ls!!1) 

fromFEN :: String -> Either ParseError Position
fromFEN = parse fen_p ""

fenpos_p :: Parsec String u Position
fenpos_p = do
	rows <- count 8 $ manyTill (digit_p <|> piece_p) (string " " <|> string "/")
	colour <- (string "w" >> return White) <|> (string "b" >> return Black)
	string " "
	castleK <- option False $ string "K" >> return True
	castleQ <- option False $ string "Q" >> return True
	castlek <- option False $ string "k" >> return True
	castleq <- option False $ string "q" >> return True
	when (not $ castleK || castleQ || castlek || castleq) $ string "-" >> return ()
	string " "
	mb_ep <- (string "-" >> return Nothing) <|> (ep_square_p >>= return . Just)
	return $ Position
		(array ((1,1),(8,8)) $ zip [ (f,r) | r <- [8,7..1], f <- [1..8] ] (concat $ concat rows))
		colour
		(if castleQ then [White] else [] ++ if castleq then [Black] else [])
		(if castleK then [White] else [] ++ if castlek then [Black] else [])
		mb_ep 0 0
	where
	digit_p = do
		d <- digit
		return $ replicate (read [d]) Nothing
	piece_p = do
		c <- oneOf "KQRBNPkqrbnp"
		return [ Just (if isUpper c then White else Black,
			head [ p | (p,[pc]) <- fENFigureChars, pc == toUpper c ]) ]
	ep_square_p = do
		fc <- satisfy (`elem` ['a'..'h'])
		rc <- string "3" <|> string "7"
		return (ord fc - ord 'a' + 1,read rc)

fen_p = do
	pos <- fenpos_p
	string " "
	halfmoves <- int_p
	string " "
	movecnt <- int_p
	return $ pos { positionHalfmoveClock = halfmoves, positionMoveCounter = movecnt }

int_p = many1 digit >>= (return . (read :: String -> Int))

{-
toPolyglotHash pos = 
--fromPolyglotHash hash =

ht = do
	putStrLn "key == toPolyglotHash pos: " ++ show (key == toPolyglotHash pos)
--	putStrLn "fromPolyglotHash key == pos" ++ show (fromPolyglotHash key == pos)
	where
	Right pos = parse fen_p "" "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR b kq - 0 3"
	key = 0x652a607ca3f242c1
-}

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
kings_coors Position{..} = head $ [ coors | (coors,Just (col,Þ)) <- assocs positionBoard, col==positionColourToMove ]

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
	loop 8 testPosition

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
				(c:_) | c `elem` "si" -> do
					let search_fun = case c of
						's' -> single_search
						'i' -> iterative_deepening
					putStrConsoleLn "Searching..."
					((val,line),s) <- runStateT (search_fun depth pos) initialSearchState
					case line of
						[] -> putStrConsoleLn $ printf "Value = %+2.2f, no move possible." val
						best_move:_ -> do
							putStrConsoleLn $ showSearchState s
							putStrConsoleLn $ "Found best move " ++ showMove pos best_move
							putStrConsoleLn $ "Best line: " ++ showLine line
							do_move best_move
				"b" -> return ()
				"q" -> error $ "Quit."
				"t" -> runTestSuite depth
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

runTestSuite depth = do
	case parseFromFile testsuite_p "BK-Test.txt" of
		Left err -> error err
		Right tests -> do
			res <- forM tests $ \ (name,pos,best_moves) -> do
				putStrConsoleLn $ printf "\n###############################################"
				putStrConsoleLn $ printf "# Test %s\n" name
				(_,bm:_) <- evalStateT (iterative_deepening depth pos) initialSearchState
				putStrConsoleLn $ printf "Found    best move  %s" (showMove pos bm)
				putStrConsoleLn $ printf "Expected best moves %s" (intercalate "," (map (showMove pos) best_moves))
				verdict <- case bm `elem` best_moves of
					False -> do
						putStrConsoleLn $ printf "=> FAIL !"
						return False
					True -> do
						putStrConsoleLn $ printf "=> OK."
						return True
				return (name,pos,bm,best_moves,verdict)

			forM_ res $ \ (name,pos,bm,best_moves,verdict) -> do
				putStrConsoleLn $ printf "Test %-7s: Expected %-5s, Found %-5s => %s" name (showMove pos bm)
					(intercalate "," (map (showMove pos) best_moves)) (if verdict then "OK" else "FAIL")

showMove_FromTo Move{..} = showCoors moveFrom ++ showCoors moveTo ++ maybe "" pieceStr movePromote

showLine :: [Move] -> String
showLine moves = intercalate ", " (map showMove_FromTo moves)

readMove pos movestr = head [ move | let Right moves = moveGenerator pos, move <- moves, movestr == showMove_FromTo move ]

data SearchState = SearchState {
	debugMode           :: Bool,
	nodesProcessed      :: Int,
	leavesProcessed     :: Int,
	evaluationsDone     :: Int,
	bestLineUpdates     :: Int,
	lastStateOutputTime :: Integer,
	bestLine            :: [Move],
	bestVal             :: Rating,
	αCutoffs            :: Int,
	βCutoffs            :: Int,
	computationProgress :: [(Int,Int)],
	killerMoveHits      :: Int,
	memoizationHits     :: Int,
	memoizationMisses   :: Int,
	positionHashtable   :: HashMap.HashMap Position (Rating,[Move]) }
	deriving (Show)
initialSearchState = SearchState False 0 0 0 0 0 [] 0.0 0 0 [] 0 0 0 HashMap.empty

showSearchState s = printf "Tot. %i Nodes, %i Leaves, %i Evals, bestLineUpdates=%i, alpha/beta-Cutoffs=%i/%i"
	(nodesProcessed s) (leavesProcessed s) (evaluationsDone s) (bestLineUpdates s) (αCutoffs s) (βCutoffs s)

comp_progress _ [] = 0.0 :: Float
comp_progress ts ((i,n):ins) = product ts * fromIntegral i / fromIntegral n + comp_progress ((1 / fromIntegral n):ts) ins

type SearchMonad a = StateT SearchState IO a

single_search depth position = do
	(res,_,_) <- do_search depth 0 position [] (-mAX_RATING,mAX_RATING) [] Nothing
	return res

iterative_deepening :: Depth -> Position -> SearchMonad (Rating,[Move])
iterative_deepening maxdepth position = do
	((rating,best_line),killermoves,(α,β)) <- do_search 4 0 position [] (-mAX_RATING,mAX_RATING) [] Nothing
	res <- iter 6 (α,β) best_line
	return res
	where
	iter m_depth (α,β) principal_var = do
		liftIO $ do
			putStrConsoleLn $ printf "\n========== iter : m_depth=%i,  maxdepth=%i" m_depth maxdepth
			putStrConsoleLn $ printf "== alpha/beta=(%+.2f,%+.2f),  principal_var: %s\n" α β (showLine principal_var)
		put initialSearchState
		(res@(_,best_line),killermoves,αβ') <- do_search m_depth 0 position [] (α,β) [] (Just principal_var)
		case m_depth >= maxdepth of
			True  -> return res
			False -> iter (m_depth+2) αβ' best_line
		

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

type KillerMoves = [Move]

numKillerMoves = 5

do_search :: Depth -> Depth -> Position -> [Move] -> (Rating,Rating) -> KillerMoves -> Maybe [Move] -> SearchMonad ((Rating,[Move]),KillerMoves,(Rating,Rating))
do_search maxdepth depth position current_line (α,β) killermoves mb_principal_var = do
--	liftIO $ putStrConsoleLn $ printf "----- CURRENT LINE: %s " (showLine current_line)
	case moveGenerator position of
		Left  _  -> return ((evalPosition position,[]),killermoves,(α,β))
		Right unsorted_moves -> do
			hashmap <- gets positionHashtable
			case HashMap.lookup position hashmap of
				Just res -> do
					modify' $ \ s -> s { memoizationHits = memoizationHits s + 1 }
					return (res,killermoves,(α,β))
				Nothing -> do
					modify' $ \ s -> s { memoizationMisses = memoizationMisses s + 1 }
					let
						principal_moves = maybe [] ((take 1) . (drop depth)) mb_principal_var
						presorted_moves = reverse $ sortBy (comparing move_sort_val) unsorted_moves
						kill_moves = killermoves `intersect` presorted_moves
						moves = principal_moves `intersect` presorted_moves ++ ((kill_moves ++ (presorted_moves \\ kill_moves)) \\ principal_moves)
					modify' $ \ s -> s { computationProgress = (0,length moves) : computationProgress s }
					(res,killer_moves',(α',β')) <- find_best_line (worst_val,[]) moves (α,β) killermoves
					modify' $ \ s -> s { positionHashtable = HashMap.insert position res (positionHashtable s) }
					debug_here depth ("find_best_line returned " ++ show res) current_line (α',β')
					modify' $ \ s -> s { computationProgress = tail (computationProgress s) }
					debug_here depth "AFTER FIND_BEST_LINE" current_line (α',β')
					return (res,killer_moves',(α',β'))

		where

		move_sort_val (Move from to mb_takes mb_promote) =
			maybe 0 (\ c -> max 1 (piecetypeval_at c - piecetypeval_at from)) mb_takes +
			maybe 0 (const 10) mb_promote
			where
			piecetypeval_at coors = let Just (_,piecetype) = (positionBoard position)!coors in 1 + index (Ù,Þ) piecetype

		(worst_val,isBetterThan,accum_fun) = case positionColourToMove position of
			White -> (α,(>),max)
			Black -> (β,(<),min)

		find_best_line :: (Rating,[Move]) -> [Move] -> (Rating,Rating) -> KillerMoves -> SearchMonad ((Rating,[Move]),KillerMoves,(Rating,Rating))
		find_best_line best@(best_val,best_line) (move:moves) (α,β) killermoves = do
			let
				current_line' = current_line ++ [move]
				depth' = depth + 1
				position' = doMove position move
			debug_here depth' ("CURRENT MOVE: " ++ showMove_FromTo move) current_line' (α,β)
			modify' $ \ s -> s { nodesProcessed = nodesProcessed s + 1 }

			((this_val,this_subline),sub_killer_moves) <- case depth' < maxdepth of
				True -> do
					(res,sub_killer_moves,_) <- do_search maxdepth depth' position' current_line' (α,β) killermoves mb_principal_var
					return (res,sub_killer_moves)
				False -> do
					modify' $ \ s -> s {
						leavesProcessed = leavesProcessed s + 1,
						nodesProcessed  = nodesProcessed  s + 1,
						evaluationsDone = evaluationsDone s + 1 }
					last_output_secs <- gets lastStateOutputTime
					TOD current_secs _ <- liftIO $ getClockTime
					when (current_secs - last_output_secs >=1) $ do
						s <- get
						liftIO $ putStrConsole $ printf "[%3.0f%%]  Cuts:%i/%i  KillerHits:%i  Memo(Pos:%i Hits:%i Miss:%i)    \r"
							(100.0 * comp_progress [] (reverse $ computationProgress s))
							(αCutoffs s) (βCutoffs s)
							(killerMoveHits s) (HashMap.size (positionHashtable s)) (memoizationHits s) (memoizationMisses s)
						modify' $ \ s -> s { lastStateOutputTime = current_secs }
					return $ ((evalPosition position',[]),[])			

			let killermoves' = take numKillerMoves $ union sub_killer_moves killermoves

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
						return (best,killermoves',(α',β'))
					_  -> find_best_line best' moves (α',β') killermoves'
				True -> do
					case positionColourToMove position of
						White -> do
							debug_here depth' "beta CUTOFF: " current_line' (α,β)
							modify' $ \ s -> s { βCutoffs = βCutoffs s + 1 }
						Black -> do
							debug_here depth' "alpha CUTOFF: " current_line' (α,β)
							modify' $ \ s -> s { αCutoffs = αCutoffs s + 1 }
					killermoves'' <- case move `elem` killermoves of
						True -> do
							modify' $ \ s -> s { killerMoveHits = killerMoveHits s + 1 }
							return killermoves'
						False -> return $ take numKillerMoves (move : killermoves')
					return (best,killermoves'',(α',β'))
