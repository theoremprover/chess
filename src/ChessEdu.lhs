%include lhs2TeX.fmt

We will use unicode symbols in the code and some language extensions...

> {-# LANGUAGE UnicodeSyntax,RecordWildCards,TypeSynonymInstances,FlexibleInstances,
>	OverlappingInstances,TupleSections,StandaloneDeriving #-}

> module Main where
> 
> import Data.Set (Set)
> import qualified Data.Set as Set
> import Data.Char
> import Data.Array
> import Data.Maybe
> import Data.List
> import Data.Stack
> import Data.NumInstances
> import Data.Ord
> import Text.Printf
> import Control.Parallel.Strategies
> import System.IO

In chess, two players

> data Colour = White | Black
> 	deriving (Show,Eq,Enum,Bounded,Ord)

are taking turns

> nextColour White = Black
> nextColour Black = White

in moving pieces

> data Piece = Ù | Ú | Û | Ü | Ý | Þ
> 	deriving (Eq,Enum,Bounded,Ord,Show)

on a board, which is an array of squares indexed by coordinates:

> type Board = Array Coors Square
> type Square = Maybe (Colour,Piece)

On a square, maybe there is a piece of a colour, or nothing.

In chess, the file coordinate is a letter

> data File = A | B | C | D | E | F | G | H
>	deriving (Show,Eq,Ix,Ord,Enum)

and the rank is an integer number.
Hence, the coordinates are the (cartesian) product

> type Coors = (File,Int)
> instance Show Coors where
>	show (file,rank) = map toLower (show file) ++ show rank

A position in chess consists of
the current board,
the colour to move,
the set of players that still have the right to castle queen side or king side,
whether a pawn could be taken en passant (double pawn step before),
and a clock counting the half moves that have been made
(chess rules say that the game is drawn if for 50+ half moves, no pawn is moved or piece is taken).
Moreover, we record the move number.

> data Position = Position {
> 	pBoard              :: Board,
> 	pColourToMove       :: Colour,
> 	pCanCastleQueenSide :: Set Colour,
> 	pCanCastleKingSide  :: Set Colour,
> 	pEnPassant          :: Maybe (Coors,Coors),
> 	pHalfmoveClock      :: Int,
>	pNextMoveNumber     :: Int }

In the initial position, White is to start,
with both players having all castling rights.
There is no pawn that could be taken en passant,
and the half move clock starts at zero:

> initialPosition = Position {
> 	pBoard = boardFromString $
> 		"âïáòäðàñ" ++
> 		"îßîßîßîß" ++
> 		"ØçØçØçØç" ++
> 		"çØçØçØçØ" ++
> 		"ØçØçØçØç" ++
> 		"çØçØçØçØ" ++
> 		"ÙèÙèÙèÙè" ++
> 		"ëÚêÝíÛéÜ",
> 	pColourToMove       = White,
> 	pCanCastleQueenSide = Set.fromList allOfThem,
> 	pCanCastleKingSide  = Set.fromList allOfThem,
> 	pEnPassant          = Nothing,
> 	pHalfmoveClock      = 0,
>	pNextMoveNumber     = 1 }

The allOfThem function generates a list of all values of the respective type,
which is needed above.

> allOfThem :: (Enum a,Bounded a,Ord a) => [a]
> allOfThem = enumFromTo minBound maxBound

Castling and promotion happen on base ranks:

> baseRank White = 1
> baseRank Black = 8

We play on a cartesian board in two dimensions with the basis {north,east}:

> (north,east) = ((0,1),(1,0))
> (south,west) = (-north,-east)

White's pawns move northwards, Black's pawns southwards.

> pawnDir White = north
> pawnDir Black = south

In order to print a board, we define a show_square function:

> show_square darksquare square = case square of 
> 	Nothing            | darksquare -> 'ç'
> 	Nothing                         -> 'Ø'
> 	Just (White,piece) | darksquare -> "èéêëìí" !! (fromEnum piece)
> 	Just (White,piece)              -> "ÙÚÛÜÝÞ" !! (fromEnum piece)
> 	Just (Black,piece) | darksquare -> "îïðñòó" !! (fromEnum piece)
> 	Just (Black,piece)              -> "ßàáâãä" !! (fromEnum piece)

read_square is the inverse of show_square, so we don't have to give both directions
and thus have single source.

> read_square :: Char -> Square
> read_square c = lookup c [ (show_square dark (Just (col,piece)), (col,piece)) |
>	col <- allOfThem, piece <- allOfThem, dark <- allOfThem ]

> boardFromString s = array ((A,1),(H,8)) $ zip [ (f,r) | r <- [8,7..1], f <- [A .. H] ] (map read_square s)

The boardFromString convenience function converts a string to
a board with pieces on it.
The resulting board is an array with an index ranging from (A,1) to (H,8),
starting from (A,8) in the upper left corner.

In order to print a chess position, we make Position an instance of Show:

> instance Show Position where
> 	show Position{..} = printf "¿ÀÀÀÀÀÀÀÀÁ\n%sÄÏÐÑÒÓÔÕÖÆ\n%s to do move %i\n"
> 		(unlines $ map show_rank [8,7..1]) (show pColourToMove) pNextMoveNumber
> 		where
> 		show_rank rank = [ "ÇÈÉÊËÌÍÎ" !! (fromEnum rank - 1) ] ++
>			[ show_square (is_darksquare (file,rank)) (pBoard!(file,rank)) | file <- [A .. H] ] ++ "Ã"
> 			where
> 			is_darksquare (file,rank) = mod (fromEnum rank + fromEnum file) 2 == 1

In order to calculate target square coordinates, we need to add deltas to coordinates.
Therefore we define a new infix operator, which is left associative and has precedence 6
(same as the normal "+" operator). 

> infixl 6 +++

The addition result maybe out of the board's bounds, hence the result type in

> (+++) :: Coors -> (Int,Int) -> Maybe Coors
> (file,rank) +++ (δfile,δrank) |
>	ifile' `elem` [0..7] && rank' `elem` [1..8] = Just (toEnum ifile',rank')
> 	where
> 	(ifile',rank') = (fromEnum file + δfile, rank + δrank)
> _  +++ _ = Nothing

A move is from a coordinate to another coordinate,
might take an opponent's piece
(for en passant, from another square than the target),
and might also promote a pawn to another piece.

> data Move =
>	Move {
> 		moveFrom    :: Coors,
> 		moveTo      :: Coors,
> 		moveTakes   :: Maybe Coors,
> 		movePromote :: Maybe Piece } |
>	Castling CastlingSide
> data CastlingSide = Queenside | Kingside
>
> instance Show Move where
> 	show Move{..} = show moveFrom ++ show moveTo ++ case movePromote of
> 		Nothing -> ""
> 		Just Ú -> "N"
> 		Just Û -> "B"
> 		Just Ü -> "R"
> 		Just Ý -> "Q"
>	show (Castling Queenside) = "O-O-O"
>	show (Castling Kingside)  = "O-O"

> doMove pos@Position{..} move = pos' {
> 	pCanCastleQueenSide = if disabled_queenside then Set.delete pColourToMove pCanCastleQueenSide else pCanCastleQueenSide,
> 	pCanCastleKingSide  = if disabled_kingside  then Set.delete pColourToMove pCanCastleKingSide  else pCanCastleKingSide,
> 	pColourToMove       = nextColour pColourToMove,

The halfmove clock increases with every consecutive move other than a pawn's and without
taking a piece. After 50 such "half"moves the match is drawn.

>	pHalfmoveClock = case move of
>		Move _    _ (Just _) _                              -> 0
>		Move from _ _        _ | Just (_,Ù) <- pBoard!from -> 0
>		_                                                   -> pHalfmoveClock + 1,
>
> 	pNextMoveNumber = pNextMoveNumber + 1,

If a pawn advances a double step, enable possibility of taking it "en passant" in the
next move by saving the pawn's intermediate and target square.

>	pEnPassant = case move of
>		Move from to Nothing Nothing |
>			Just (_,Ù) <- pBoard!from,
>			Just to == from +++ 2*pawn_dir,
>			Just middle <- from +++ pawn_dir -> Just (middle,to)
>		_                                    -> Nothing }
> 	where
>	pawn_dir = pawnDir pColourToMove 
>	(disabled_queenside,disabled_kingside) = case (pColourToMove,move) of
> 		(_,Castling _)           -> (True, True )
> 		(White,Move (E,1) _ _ _) -> (True, True )
> 		(Black,Move (E,8) _ _ _) -> (True, True )
> 		(White,Move (A,1) _ _ _) -> (True, False)
> 		(Black,Move (A,8) _ _ _) -> (True ,False)
> 		(White,Move (H,1) _ _ _) -> (False,True )
> 		(Black,Move (H,8) _ _ _) -> (False,True )
>		_					     -> (False,False)
> 	pos' = pos { pBoard = pBoard // case move of
> 		Move{..} -> maybe [] (\ take_coors -> [(take_coors,Nothing)]) moveTakes ++
>			[ (moveFrom,Nothing), (moveTo,maybe (pBoard!moveFrom) (Just.(pColourToMove,)) movePromote) ]
> 		Castling Queenside -> [ ((A,r),Nothing), ((D,r),pBoard!(A,r)), ((E,r),Nothing), ((C,r),pBoard!(E,r)) ]
> 		Castling Kingside  -> [ ((H,r),Nothing), ((F,r),pBoard!(H,r)), ((E,r),Nothing), ((G,r),pBoard!(E,r)) ] } where
>		r = baseRank pColourToMove

In a chess match, either one colour checkmates or it is a draw for some reason
(one could also resign, of course...)

> data MatchResult = Winner Colour WinReason | Draw DrawReason deriving Show
> data WinReason  = Resignation | Checkmate deriving Show
> data DrawReason = Fifty_Halfmoves | Stalemate | NoWinPossible deriving Show

The move generator generates all legal moves in a position by first calculating
all potential moves and then filtering out the moves that are not allowed
because the king would be in check.

> moveGen pos@Position{..} = filter king_not_in_check $ potentialMoves pos
>	where
> 	king_not_in_check move = all (coorsNotInCheck pos_after_move pColourToMove) $ case move of
>		Castling Queenside -> [(E,r),(D,r),(C,r)]
>		Castling Kingside  -> [(E,r),(F,r),(G,r)]
>		Move{..}           -> [ kingsCoors pos_after_move pColourToMove ]
>		where
>		r = baseRank pColourToMove
>		pos_after_move = doMove pos move

Is a square threatened by a piece of a given colour?

> coorsNotInCheck pos colour coors = all (/=coors) [ moveTo |
> 	Move{..} <- potentialMoves $ pos {
> 		-- Say, the next colour would be to move now...
> 		pColourToMove = nextColour colour,
>		-- ... and place some figure at the coors to also get pawn takes as potential moves:
> 		pBoard = pBoard pos // [ (coors,Just (colour,Ý)) ] } ]

> notInCheck pos@Position{..} = coorsNotInCheck pos pColourToMove $ kingsCoors pos pColourToMove

> kingsCoors pos colour = head [ coors | (coors,Just (col,Þ)) <- assocs (pBoard pos), col == colour ]

> potentialMoves pos@Position{..} = [ Move move_from move_to mb_takes mb_promote |
>	(move_from@(_,from_rank),Just (colour,piece)) <- assocs pBoard, colour==pColourToMove,
>	(move_to@(_,to_rank),mb_takes) <- let
>		initial_rank = if pColourToMove==White then 2 else 7
>		pawn_dir = pawnDir pColourToMove 
> 		diagonals = [ north+east,north+west,south+east,south+west ]
> 		straights = [ north,west,south,east ]
>	 	knight_moves = [ north+2*east,north+2*west,2*north+west,2*north+east,south+2*west,south+2*east,2*south+west,2*south+east ]
>		maybe_move from δ = case from +++ δ of
>			Nothing -> []
>			Just move_to -> case pBoard!move_to of
>				Nothing                             -> [ (move_to,Nothing) ]
>				Just (col,_) | col /= pColourToMove -> [ (move_to,Just move_to) ]
>				_            | otherwise            -> []
>		maybe_move_direction from δ = case maybe_move from δ of
>			move@[ (_,Just _)   ] -> move
>			move@[ (to,Nothing) ] -> move ++ maybe_move_direction to δ
>			_ | otherwise         -> []
>		in case piece of
>			Ù -> ( case move_from +++ pawn_dir of

If the square in front of the pawn direction is empty, it could move there:

>				Just move_to | Nothing <- pBoard!move_to -> [ (move_to,Nothing) ] ++

If, moreover, the pawn still stands on its initial position, it could even move two squares
given that this target square is also empty:

>					case move_from +++ 2*pawn_dir of
>						Just move_to2 | Nothing <- pBoard!move_to2, from_rank==initial_rank -> [ (move_to2,Nothing) ]
>						_ -> []
>				_            | otherwise -> [] ) ++

A pawn can take pieces diagonally in front of it,
even intercepting a two square move of an opponent's pawn ("en passant"):

>				[ (move_to,Just take_on) | Just move_to <- map (move_from +++) [pawn_dir+east,pawn_dir+west],
>					take_on <- case pBoard!move_to of

If there is an opponent's piece on target square, one can take it:

>						Just (colour,_) | colour /= pColourToMove                                   -> [ move_to ]

If en passant is enabled with the middle square being the current move's target, one can also take:

>						_               | Just (middle,pawn_coors) <- pEnPassant, middle == move_to -> [ pawn_coors ]
>						_               | otherwise                                                 -> [] ]

For a knight, there is a fixed set of target squares.

>			Ú -> concatMap (maybe_move           move_from) knight_moves

A bishop can move from its location in each of the diagonals' direction:

>			Û -> concatMap (maybe_move_direction move_from) diagonals

Same for rooks, but in straight direction:

>			Ü -> concatMap (maybe_move_direction move_from) straights

The queen can move is each direction.

>			Ý -> concatMap (maybe_move_direction move_from) (straights++diagonals)

The king can only move one step, but also in each direction.

>			Þ -> concatMap (maybe_move           move_from) (straights++diagonals),

Only a pawn will be promoted to a piece once it reaches the opposite base rank:

>	mb_promote <- case piece of
>		Ù | to_rank == baseRank (nextColour pColourToMove) -> map Just [Ý,Ú,Û,Ü]
>		_ | otherwise                                      -> [ Nothing ] ] ++
>
>	let
>		r = baseRank pColourToMove
>		square_empty coors = isNothing $ pBoard!coors
>	in

A player might castle kingside or queenside, if both the king and the rook haven't moved yet
(i.e. the player's colour is stored in pCanCastle<X>Side) and the squares between them are empty.

>	[ Castling Kingside  | pColourToMove `Set.member` pCanCastleKingSide,  all square_empty [(F,r),(G,r)] ] ++
>	[ Castling Queenside | pColourToMove `Set.member` pCanCastleQueenSide, all square_empty [(D,r),(C,r),(B,r)] ]

The rating of a position is a float number with a minimum and a maximum rating.
An equal postion's rating is 0.

> type Rating = Float
> mAX         = 10000.0
> mIN         = negate mAX
> eQUAL       =     0.0

The rate function will return a rating of the position together
with an match result if the match ends.

> rate :: Position -> (Rating,Maybe MatchResult)

If there is no pawn moved or piece taken for 50 halfmoves,
the game is drawn:

> rate Position{..} | pHalfmoveClock >= 50 = (eQUAL,Just $ Draw Fifty_Halfmoves)

If there is no legal move for one colour, we have either a stalemate,
or a checkmate if the king is in check:

> rate pos | [] <- moveGen pos = case (notInCheck pos,pColourToMove pos) of
> 	(True,_)      -> (eQUAL,Just $ Draw Stalemate)
> 	(False,White) -> (mIN,  Just $ Winner Black Checkmate)
> 	(False,Black) -> (mAX,  Just $ Winner White Checkmate)

With only one bishop or knight ("light figures"), neither side can win:

> rate pos@Position{..} | max_one_light_figure = (eQUAL,Just $ Draw NoWinPossible) where
> 	all_light_figures = all (`elem` [Ú,Û])
> 	max_one_light_figure = case sort $ filter ((/=Þ).snd) $ catMaybes $ elems pBoard of
> 		[]                                                          -> True
> 		[(_,fig)]                   | all_light_figures [fig]       -> True
> 		[(White,fig1),(Black,fig2)] | all_light_figures [fig1,fig2] -> True
>		_                           | otherwise                     -> False

Otherwise, we will rate the position based on material, distance of pawns from promotion,
and the mobility of each piece.

> rate pos = (rating,Nothing) where
>	rating = 0.01*mobility + sum [ (if colour==White then id else negate) (piece_val piece colour coors) |
> 		(coors,Just (colour,piece)) <- assocs $ pBoard pos ]
> 	piece_val Ù colour (_,rank) = 1 + case abs (rank - baseRank (nextColour colour)) of
> 				1             -> 4
> 				2             -> 2
> 				_ | otherwise -> 0
> 	piece_val Ú _ _ = 3
> 	piece_val Û _ _ = 3
> 	piece_val Ü _ _ = 5
> 	piece_val Ý _ _ = 9
> 	piece_val Þ _ _ = 0
> 	mobility = fromIntegral $
> 		length (potentialMoves $ pos { pColourToMove = White }) -
> 		length (potentialMoves $ pos { pColourToMove = Black })

The search function calculates the best move up to a certain depth according
to the minimax algortihm.
We represent the computing depth as an integer.

> type Depth = Int
>
> search :: Bool -> Depth -> Position -> [Move] -> (Rating,[Move])
> search _ maxdepth pos                     line | null (moveGen pos) || maxdepth==0 = (fst $ rate pos,line)
> search parallel maxdepth pos@Position{..} line = minimax (comparing fst) (functor deeper $ moveGen pos)
> 	where

Since with pure functional code there can't be any race conditions or deadlocks,
we can easily distribute the search on multicore by using a parallel map:

>	functor     = if parallel then parMap rpar else map
> 	minimax     = if pColourToMove == White then maximumBy else minimumBy
> 	deeper move = search False (maxdepth-1) (doMove pos move) (move:line)

In order to play a match vs. the computer, we need an interaction loop accepting input
from the console.

> main = loop 2 initialPosition stackNew where
>	loop :: Depth -> Position -> Stack Position -> IO ()
>	loop maxdepth pos pos_history = do
>		print pos
>		putStrLn $ printf "Rating = %.2f" (fst $ rate pos)
>		putStrLn $ "Possible moves are:" ++ show (moveGen pos)
>		case rate pos of

Note that in the rate function call above, the actual rating number won't be computed because it is not needed
("lazy evaluation") because is only matched against wildcards below:

>			(_,Just matchresult) -> print matchresult
>			_ | otherwise        -> return ()
>		input <- putStr "? " >> hFlush stdout >> getLine
>		case input of
>			"i" -> loop maxdepth initialPosition stackNew
>			"q" -> return ()
>			"s" -> execute_move $ last $ snd $ search False maxdepth pos []
>			"p" -> execute_move $ last $ snd $ search True  maxdepth pos []
>			"b" -> case stackPop pos_history of
>				Nothing -> do
>					putStrLn "There is no previous position."
>					loop maxdepth pos pos_history
>				Just (stack',prev_pos) -> loop maxdepth prev_pos stack'
>			depth_str | [(depth,[])] <- reads depth_str -> loop depth pos pos_history
>			move_str -> case lookup move_str $ map (\ m -> (show m,m)) $ moveGen pos of
>				Nothing   -> do
>					putStrLn "This is no (legal) move or command."
>					loop maxdepth pos pos_history
>				Just move -> execute_move move
>		where
>		execute_move move = loop maxdepth (doMove pos move) (stackPush pos_history pos)
