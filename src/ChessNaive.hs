--We will use unicode symbols in the code and some language extensions...

{-# LANGUAGE UnicodeSyntax,RecordWildCards,TypeSynonymInstances,FlexibleInstances,
	OverlappingInstances,TupleSections,StandaloneDeriving,DeriveGeneric #-}

module Main where

import Data.Char
import Data.Array
import Data.Maybe
import Data.List
import Data.Stack
import Data.NumInstances
import Data.Ord
import Text.Printf
import System.IO
import System.CPUTime
import System.Time

data Colour = White | Black
	deriving (Show,Eq,Enum,Bounded,Ord)

nextColour White = Black
nextColour Black = White

data Piece = Ù | Ú | Û | Ü | Ý | Þ
	deriving (Eq,Enum,Bounded,Ord,Show)

type Board  = Array Coors Square
type Square = Maybe (Colour,Piece)

data File = A | B | C | D | E | F | G | H
	deriving (Show,Eq,Ix,Ord,Bounded,Enum)
data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
	deriving (Show,Eq,Ix,Ord,Bounded,Enum)
type Coors = (File,Rank)
instance Show Coors where
	show (file,rank) = map toLower (show file) ++ drop 1 (show rank)

data Position = Position {
	pBoard              :: Board,
	pColourToMove       :: Colour,
	pCanCastleQueenSide :: [Colour],
	pCanCastleKingSide  :: [Colour],
	pEnPassant          :: Maybe (Coors,Coors),
	pHalfmoveClock      :: Int,
	pNextMoveNumber     :: Int }

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

allOfThem :: (Enum a,Bounded a,Ord a) => [a]
allOfThem = [minBound..maxBound]

baseRank White = R1
baseRank Black = R8

(north,east) = ((0,1),(1,0))
(south,west) = (-north,-east)

pawnStep White = north
pawnStep Black = south

show_square darksquare square = case square of 
	Nothing            | darksquare -> 'ç'
	Nothing                         -> 'Ø'
	Just (White,piece) | darksquare -> "èéêëìí" !! (fromEnum piece)
	Just (White,piece)              -> "ÙÚÛÜÝÞ" !! (fromEnum piece)
	Just (Black,piece) | darksquare -> "îïðñòó" !! (fromEnum piece)
	Just (Black,piece)              -> "ßàáâãä" !! (fromEnum piece)

read_square c = lookup c [ (show_square dark (Just (col,piece)), (col,piece)) |
	col <- allOfThem, piece <- allOfThem, dark <- allOfThem ]

boardFromString s = array (minBound,maxBound) $ zip [ (f,r) | r <- reverse allOfThem, f <- allOfThem ] (map read_square s)
instance Show Position where
	show Position{..} = printf "¿ÀÀÀÀÀÀÀÀÁ\n%sÄÏÐÑÒÓÔÕÖÆ\n%s to do move %i\n"
		(unlines $ map show_rank (reverse allOfThem)) (show pColourToMove) pNextMoveNumber
		where
		show_rank rank = [ "ÇÈÉÊËÌÍÎ" !! (fromEnum rank) ] ++
			[ show_square (is_darksquare (file,rank)) (pBoard!(file,rank)) | file <- allOfThem ] ++ "Ã"
			where
			is_darksquare (file,rank) = mod (fromEnum rank + fromEnum file) 2 == 0

infixl 6 +++
(+++) :: Coors -> (Int,Int) -> Maybe Coors
(file,rank) +++ (δfile,δrank) = case ifile' `elem` fileindices && irank' `elem` rankindices of
	False -> Nothing
	True  -> Just (toEnum ifile',toEnum irank')
	where
	fileindices = map fromEnum (allOfThem::[File])
	rankindices = map fromEnum (allOfThem::[Rank])
	(ifile',irank') = (fromEnum file + δfile, fromEnum rank + δrank)

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

doMove pos@Position{..} move = pos {

	pBoard              = pBoard // case move of
		Move{..} -> ( case moveTakes of
			Nothing -> []
			Just take_coors -> [(take_coors,Nothing)] ) ++
			[ (moveFrom,Nothing), (moveTo,case movePromote of
				Nothing         -> pBoard!moveFrom
				Just promote_to -> Just (pColourToMove,promote_to) ) ]
		Castling Queenside -> [ ((A,r),Nothing), ((D,r),pBoard!(A,r)), ((E,r),Nothing), ((C,r),pBoard!(E,r)) ]
		Castling Kingside  -> [ ((H,r),Nothing), ((F,r),pBoard!(H,r)), ((E,r),Nothing), ((G,r),pBoard!(E,r)) ],

	pCanCastleQueenSide = (if forfeit_queenside then delete pColourToMove else id) pCanCastleQueenSide,
	pCanCastleKingSide  = (if forfeit_kingside  then delete pColourToMove else id) pCanCastleKingSide,
	pColourToMove       = nextColour pColourToMove,

	pHalfmoveClock      = case move of
		Move {..} | Just _      <- moveTakes       -> 0
		Move {..} | Just (_,Ù) <- pBoard!moveFrom -> 0
		_         | otherwise                      -> pHalfmoveClock + 1,

	pNextMoveNumber     = if pColourToMove==Black then pNextMoveNumber+1 else pNextMoveNumber,

	pEnPassant          = case move of
		Move from to Nothing Nothing |
			Just (_,Ù) <- pBoard!from,
			Just to == from +++ 2*pawn_step,
			Just middle <- from +++ pawn_step -> Just (middle,to)
		_ | otherwise                        -> Nothing }

	where

	pawn_step = pawnStep pColourToMove
	r = baseRank pColourToMove
	(forfeit_queenside,forfeit_kingside) = case move of
		Castling _                   -> (True, True )
		Move (A,rank) _ _ _ | rank == r -> (True, False)
		Move (E,rank) _ _ _ | rank == r -> (True, True )
		Move (H,rank) _ _ _ | rank == r -> (False,True )
		_ | otherwise                   -> (False,False)

data MatchResult = Winner Colour WinReason | Draw DrawReason deriving Show
data WinReason  = Resignation | Checkmate deriving Show
data DrawReason = Fifty_Halfmoves | Stalemate | NoWinPossible deriving Show

moveGen pos@Position{..} = filter king_not_in_check $ potentialMoves pos where
	king_not_in_check move = all (coorsNotInCheck pos_after_move pColourToMove) $ case move of
		Move{..}           -> [ kingsCoors pos_after_move pColourToMove ]
		Castling Queenside -> [(E,r),(D,r),(C,r)]
		Castling Kingside  -> [(E,r),(F,r),(G,r)]
		where
		r = baseRank pColourToMove
		pos_after_move = doMove pos move

coorsNotInCheck pos colour coors = all (/=coors) [ moveTo |
	Move{..} <- potentialMoves $ pos {
		pColourToMove = nextColour colour,
		pBoard = pBoard pos // [ (coors,Just (colour,Ý)) ] } ]

kingsCoors pos colour = head [ coors | (coors,Just (col,Þ)) <- assocs (pBoard pos), col == colour ]

potentialMoves pos@Position{..} = normal_moves ++ castling_moves where
	normal_moves = [ Move src dest mb_takes mb_promote |
		(src,Just (colour,piece)) <- assocs pBoard,
		colour==pColourToMove,
		(dest@(_,to_rank),mb_takes) <- let
			initial_rank = if pColourToMove==White then R2 else R7
			pawn_step = pawnStep pColourToMove 
			diagonals    = [ north+east,north+west,south+east,south+west ]
			straights    = [ north,west,south,east ]
			knight_moves = [ north+2*east,north+2*west,2*north+west,2*north+east,south+2*west,south+2*east,2*south+west,2*south+east ]
			maybe_move from δ = case from +++ δ of
				Nothing -> []
				Just dest -> case pBoard!dest of
					Nothing                             -> [ (dest,Nothing) ]
					Just (col,_) | col /= pColourToMove -> [ (dest,Just dest) ]
					_            | otherwise            -> []
			maybe_move_direction from δ = case maybe_move from δ of
				move@[ (_,Just _)   ] -> move
				move@[ (to,Nothing) ] -> move ++ maybe_move_direction to δ
				_ | otherwise         -> []
			in case piece of
				Ù -> ( case src +++ pawn_step of
					Just dest | square_empty dest -> [ (dest,Nothing) ] ++
						case src +++ 2*pawn_step of
							Just dest2 | square_empty dest2, snd src == initial_rank -> [ (dest2,Nothing) ]
							_ -> []
					_         | otherwise -> [] ) ++
					[ (dest,Just take_on) | Just dest <- map (src +++) [pawn_step+east,pawn_step+west],
						take_on <- case pBoard!dest of
							Just (colour,_) | colour /= pColourToMove                                -> [ dest ]
							_               | Just (middle,pawn_coors) <- pEnPassant, middle == dest -> [ pawn_coors ]
							_               | otherwise                                              -> [] ]
				Ú -> concatMap (maybe_move           src) knight_moves
				Û -> concatMap (maybe_move_direction src) diagonals
				Ü -> concatMap (maybe_move_direction src) straights
				Ý -> concatMap (maybe_move_direction src) (straights++diagonals)
				Þ -> concatMap (maybe_move           src) (straights++diagonals),
		mb_promote <- case piece of
			Ù | to_rank == baseRank (nextColour pColourToMove) -> map Just [Ý,Ú,Û,Ü]
			_ | otherwise                                      -> [ Nothing ] ]
	castling_moves = [ Castling Kingside  | pColourToMove `elem` pCanCastleKingSide,  all square_empty [(F,r),(G,r)] ] ++
		             [ Castling Queenside | pColourToMove `elem` pCanCastleQueenSide, all square_empty [(D,r),(C,r),(B,r)] ]
	r = baseRank pColourToMove
	square_empty coors = isNothing $ pBoard!coors

type Rating = Float
mAX         = 10000.0
mIN         = negate mAX
eQUAL       =     0.0

rate :: Position -> (Rating,Maybe MatchResult)
rate Position{..} | pHalfmoveClock >= 50 = (eQUAL,Just $ Draw Fifty_Halfmoves)
rate pos@Position{..} | moveGen pos == [] = case (not_in_check,pColourToMove) of
	(True ,_    ) -> (eQUAL,Just $ Draw Stalemate)
	(False,White) -> (mIN,  Just $ Winner Black Checkmate)
	(False,Black) -> (mAX,  Just $ Winner White Checkmate)
	where
	not_in_check = coorsNotInCheck pos pColourToMove $ kingsCoors pos pColourToMove

rate pos@Position{..} | max_one_light_figure = (eQUAL,Just $ Draw NoWinPossible) where
	max_one_light_figure = case sort $ filter ((/=Þ).snd) $ catMaybes $ elems pBoard of
		[]                                                          -> True
		[(_,fig)]                   | all_light_figures [fig]       -> True
		[(White,fig1),(Black,fig2)] | all_light_figures [fig1,fig2] -> True
		_                           | otherwise                     -> False
	all_light_figures = all (`elem` [Ú,Û])
rate pos = (rating,Nothing) where
	rating = 0.01*mobility + sum [ (if colour==White then id else negate) (piece_val piece colour coors) |
		(coors,Just (colour,piece)) <- assocs $ pBoard pos ]
	piece_val Ù colour (_,rank) = 1 + case abs (fromEnum rank - fromEnum (baseRank (nextColour colour))) of
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

type Depth = Int
type Line = [Move]

search :: Depth -> Position -> Line -> (Rating,Line)
search depth pos              line | moveGen pos == [] || depth==0 = (fst $ rate pos,line)
search depth pos@Position{..} line = minimax (comparing fst) (map deeper $ moveGen pos) where
	minimax     = if pColourToMove == White then maximumBy else minimumBy
	deeper move = search (depth-1) (doMove pos move) (move:line)

main = loop 2 initialPosition stackNew where
	loop :: Depth -> Position -> Stack Position -> IO ()
	loop maxdepth pos pos_history = do
		print pos
		putStrLn $ printf "Rating = %.2f" (fst $ rate pos)
		putStrLn $ "Possible moves are:" ++ show (moveGen pos)
		case rate pos of
			(_,Just matchresult) -> print matchresult
			_ | otherwise        -> return ()
		input <- putStr "? " >> hFlush stdout >> getLine
		case input of
			"i" -> loop maxdepth initialPosition stackNew
			"q" -> return ()
			"s" -> execute_move $ last $ snd $ search maxdepth pos []
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
