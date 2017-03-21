{-# LANGUAGE UnicodeSyntax,RecordWildCards,TypeSynonymInstances,FlexibleInstances,OverlappingInstances #-}

module Main where

import Data.Array
import Data.Maybe
import Data.Ix
import Data.Char
import Data.List
import Data.Tuple
import Data.NumInstances
import System.Random

data Piece = Ù | Ú | Û | Ü | Ý | Þ deriving (Show,Eq,Enum,Ord)
data Colour = White | Black deriving (Show,Eq,Enum,Ord)
nextColour White = Black
nextColour Black = White
data File = A | B | C | D | E | F | G | H
	deriving (Show,Ix,Ord,Eq,Enum)
data Rank = First | Second | Third | Fourth | Fifth | Sixth | Seventh | Eighth
	deriving (Ix,Ord,Eq,Enum)
instance Show Rank where
	show rank = show $ index (First,Eighth) rank + 1
type Coors = (File,Rank)
type Board = Array Coors (Maybe (Colour,Piece))
data Position = Position {
	pBoard              :: Board,
	pColourToMove       :: Colour,
	pCanCastleQueenSide :: [Colour],
	pCanCastleKingSide  :: [Colour],
	pEnPassantSquare    :: Maybe Coors,
	pHalfmoveClock      :: Int,
	pMoveCounter        :: Int }
	deriving (Eq)

initialPosition = Position {
	pBoard = boardFromString [
		"âïáòäðàñ",
		"îßîßîßîß",
		"ØçØçØçØç",
		"çØçØçØçØ",
		"ØçØçØçØç",
		"çØçØçØçØ",
		"ÙèÙèÙèÙè",
		"ëÚêÝíÛéÜ" ],
	pColourToMove       = White,
	pCanCastleQueenSide = [White,Black],
	pCanCastleKingSide  = [White,Black],
	pEnPassantSquare    = Nothing,
	pHalfmoveClock      = 0,
	pMoveCounter        = 0 }

boardFromString s = listArray ((A,First),(H,Eighth)) $ map to_fig $ concat $ map reverse $ transpose s
	where
	to_fig c_rep = case if c_rep < 'ç' then c_rep else chr $ ord c_rep - ord 'ç' + ord 'Ø' of
		c | c `elem` "ÙÚÛÜÝÞ" -> Just (White,toEnum (ord c - ord 'Ù'))
		c | c `elem` "ßàáâãä" -> Just (Black,toEnum (ord c - ord 'ß'))
		_ -> Nothing

instance Show Position where
	show Position{..} = unlines $
		[ "¿ÀÀÀÀÀÀÀÀÁ" ] ++
		map show_rank [Eighth,Seventh .. First] ++
		[ "ÄÏÐÑÒÓÔÕÖÆ" ] ++
		[ show pColourToMove ++ " to move" ]
		where
		show_rank rank = [ "ÇÈÉÊËÌÍÎ" !! fromEnum rank ] ++
			map (show_square rank) [A .. H] ++ "Ã"
		show_square rank file = case pBoard!(file,rank) of
				Nothing -> if darksquare then 'ç' else 'Ø'
				Just (White,piece) | darksquare -> "èéêëìí" !! (fromEnum piece)
				Just (White,piece)              -> "ÙÚÛÜÝÞ" !! (fromEnum piece)
				Just (Black,piece) | darksquare -> "îïðñòó" !! (fromEnum piece)
				Just (Black,piece)              -> "ßàáâãä" !! (fromEnum piece)
			where
			darksquare = mod (fromEnum rank + fromEnum file) 2 == 0

infixl 6 +@
(+@) :: Maybe Coors -> (Int,Int) -> Maybe Coors
(Just (file,rank)) +@ (δfile,δrank) | ifile `elem` [0..7] && irank `elem` [0..7] =
	Just (toEnum ifile,toEnum irank)
	where
	(ifile,irank) = (fromEnum file + δfile,fromEnum rank + δrank)
_ +@ _ = Nothing

instance Show Coors where
	show (file,rank) = map toLower $ show file ++ show (fromEnum rank + 1)

data Move = Move {
	moveFrom :: Coors, moveTo :: Coors, moveTakes :: Maybe Coors, movePromote :: Maybe Piece }
	deriving Eq
instance Show Move where
	show Move{..} = show moveFrom ++ show moveTo ++ case movePromote of
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

-- is colour's move igoring check?
legal_move_wrt_check pos move = all (coors_not_in_check pos (pColourToMove pos)) $ case moveIsCastling pos move of
	Just (Left White) ->  [(E,First), (D,First), (C,First)]
	Just (Right White) -> [(E,First), (F,First), (G,First)]
	Just (Left Black) ->  [(E,Eighth),(D,Eighth),(C,Eighth)]
	Just (Right Black) -> [(E,Eighth),(F,Eighth),(G,Eighth)]
	Nothing -> [ kings_coors (doMove pos move) (pColourToMove pos) ]

-- Checks is the player to move is in check
no_check pos = coors_not_in_check pos colour (kings_coors pos colour) where
	colour = pColourToMove pos

-- Coors of colour's king
kings_coors pos colour = head [ coors | (coors,Just (col,Þ)) <- assocs (pBoard pos), col == colour ]

-- Checks if colour's coors are threatened by the other player
coors_not_in_check pos colour coors = all ((/=coors).moveTo) moves
	where
	board' = pBoard pos // [ (coors,Just (colour,Þ)) ] -- Place a figure on the square to also get pawn takes
	moves = moveTargets $ pos { pColourToMove = nextColour (pColourToMove pos), pBoard = board' }

{--###########

-- Che

no_check_after_move :: Position -> Move -> Bool

coors_in_check (pos,colour_to_move,coors) =

--------}

moveTargets pos@Position{..} = concatMap piece_moves (assocs pBoard)
	where
	(north,south,east,west) = ((0,1),(0,-1),(1,0),(-1,0))
	diagonal = [ north+east,north+west,south+east,south+west ]
	straight = [ north,west,south,east ]
	piece_moves (from,Just (col,piece)) | col == pColourToMove = case piece of
		Ù -> maybe_move from ((Just from) +@ pawn_dir) ++
			(case (Just from) +@ pawn_dir of
				Just to -> case pBoard!to of
					Nothing | snd from == pawn_row -> maybe_move from ((Just from) +@ 2*pawn_dir)
					_ -> []) ++
			concat [ maybe_take from ((Just from) +@ (pawn_dir+eastwest)) | eastwest <- [east,west] ]
		Ú -> concatMap (try_move_to from) [ north+2*east,north+2*west,2*north+west,2*north+east,south+2*west,south+2*east,2*south+west,2*south+east ]
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
	try_move_to from δ = maybe_move from ((Just from) +@ δ) ++ maybe_take from ((Just from) +@ δ)

	maybe_take from (Just to) = case pBoard!to of
		Just (col,_) | col /= pColourToMove   -> [ Move from to (Just to) promo | promo <- promotions from to ]
		Nothing | pEnPassantSquare == Just to -> [ Move from to ep_pawn Nothing ] where
			ep_pawn = pEnPassantSquare +@ ( if pColourToMove==White then south else north )
		_ -> []
	maybe_take _ _ = []

	maybe_move from (Just to) = case pBoard!to of
		Nothing -> [ Move from to Nothing promo | promo <- promotions from to ]
		_ -> []
	maybe_move _ _ = []

	promotions from to = case (pBoard!from,to) of
		(Just (_,Ù),(_,Eighth)) | pColourToMove==White -> map Just [Ú,Û,Ü,Ý]
		(Just (_,Ù),(_,First))  | pColourToMove==Black -> map Just [Ú,Û,Ü,Ý]
		_ -> [ Nothing ]

	(base_row,pawn_row) = if pColourToMove == White then (First,Second) else (Eighth,Seventh)
	isEmpty coors = isNothing (pBoard!coors)
	
doMove Position{..} Move{..} = Position {
	pBoard				= pBoard // ( mb_take ++ move moveFrom moveTo ++ mb_castling ),
	pColourToMove       = nextColour pColourToMove,
	pCanCastleQueenSide = (if no_castling_queenside_any_more then delete pColourToMove else id) pCanCastleQueenSide,
	pCanCastleKingSide  = (if no_castling_kingside_any_more  then delete pColourToMove else id) pCanCastleKingSide,
	pEnPassantSquare    = case (moved_piece,moveFrom,moveTo) of
		(Ù,(file,Second), (_,Fourth)) -> Just (file,Third)
		(Ù,(file,Seventh),(_,Fifth )) -> Just (file,Sixth)
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

	(no_castling_queenside_any_more,no_castling_kingside_any_more) = case (moveFrom,pColourToMove) of
		_ | moved_piece==Þ -> (True, True )
		((H,First), White) -> (False,True )
		((A,First), White) -> (True ,False)
		((H,Eighth),Black) -> (False,True )
		((A,Eighth),Black) -> (True ,False)
		_                  -> (False,False)

type Rating = Float
wHITE_MATE = -10000.0 :: Float
bLACK_MATE =  10000.0 :: Float
dRAW       =      0.0 :: Float
sTALEMATE  = dRAW

rate :: Position -> Rating
rate Position{..} | pHalfmoveClock >= 50 = dRAW
rate pos | null (moveGen pos) = case no_check pos of
	False -> if pColourToMove pos == White then wHITE_MATE else bLACK_MATE
	True -> sTALEMATE
rate pos | max_one_light_figure pos = dRAW
rate pos = 0.1*mobility + sum [ (if colour==White then 1 else -1) * piece_val |
	(coors@(file,rank),Just (colour,piece)) <- assocs (pBoard pos),
	let piece_val = case piece of
		Ù -> 1 + case distance coors (file,if colour==White then Eighth else First) of
			1 -> 4
			2 -> 2
			_ -> 0
		Ú -> 3
		Û -> 3
		Ü -> 5
		Ý -> 9
		Þ -> 0 ]
	where
	distance (file1,rank1) (file2,rank2) =
		max (abs (fromEnum file1 - fromEnum file2)) (abs (fromEnum rank1 - fromEnum rank2))
	mobility :: Rating
	mobility = fromIntegral $ length (moveTargets pos) -
		length (moveTargets (pos { pColourToMove = nextColour (pColourToMove pos) }))

max_one_light_figure Position{..} = case sort $ catMaybes $ elems pBoard of
	[(White,Þ),(Black,Þ)]                                                   -> True
	[(_,fig),(White,Þ),(Black,Þ)]             | light_figures [fig]         -> True
	[(_,fig_w),(White,Þ),(_,fig_b),(Black,Þ)] | light_figures [fig_w,fig_b] -> True
	where
	light_figures = all (`elem` [Ú,Û])

main = do
	loop [initialPosition]

loop poss@(pos:lastpos) = do
	print pos
	let moves = moveGen pos
	print moves
	putStr "> "
	s <- getLine
	case s of
		"random" -> randomMatch pos
		"q" -> return ()
		"r" -> do
			putStrLn $ "Rating: " ++ show (rate pos)
			loop poss
		"b" -> loop lastpos
		m -> case filter ((m==).snd) (zip moves (map show moves)) of
			[] -> do
				putStrLn "No move."
				loop poss
			[(move,_)] -> loop (doMove pos move : poss)
	where
	randomMatch pos = case (rate pos) `elem` [dRAW,] of
		dRAW
	randomMatch pos = do
		let moves = moveGen pos
		case moves of
			[] -> do
				putStrLn "Game over"
			_ -> do
				r <- randomIO
				let move = moves!!(mod r (length moves))
				putStrLn $ "Moving " ++ show move
				let pos' = doMove pos move
				print pos'
				randomMatch pos'
		