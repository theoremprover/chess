{-# LANGUAGE UnicodeSyntax,RecordWildCards,TypeSynonymInstances,FlexibleInstances,OverlappingInstances #-}

module Main where

import Data.Array
import Data.Maybe
import Data.Ix
import Data.Char
import Data.List
import Data.Tuple
import Data.NumInstances

data Piece = Ù | Ú | Û | Ü | Ý | Þ deriving (Show,Eq,Enum)
data Colour = White | Black deriving (Show,Eq,Enum)
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
	show Move{..} = show moveFrom ++ (if isJust moveTakes then "x" else "") ++ show moveTo ++ case movePromote of
		Nothing -> ""
		Just Ú -> "N"
		Just Û -> "B"
		Just Ü -> "R"
		Just Ý -> "Q"

moveGen pos = filter leads_not_to_king_sack $ moveTargets pos
	where
	leads_not_to_king_sack move = not $ any king_sack $ moveTargets pos' where
		pos' = doMove move pos
		king_sack (Move _ _ (Just takes) _) = case (pBoard pos')!takes of
			Just (_,Þ) -> True
			_ -> False

moveTargets pos@Position{..} = concatMap piece_moves (assocs pBoard)
	where
	(north,south,east,west) = ((0,1),(0,-1),(1,0),(-1,0))
	diagonal = [ north+east,north+west,south+east,south+west ]
	straight = [ north,west,south,east ]
	piece_moves (from,Just (col,piece)) | col == pColourToMove = case piece of
		Ù ->
			maybe_move from ((Just from) +@ pawn_dir) ++
			(case (Just from) +@ pawn_dir of
				Just to -> case pBoard!to of
					Nothing -> maybe_move from ((Just from) +@ 2*pawn_dir)
					_ -> []) ++
			concat [ maybe_take from ((Just from) +@ (pawn_dir+eastwest)) | eastwest <- [east,west] ]
		Ú -> concatMap (try_move_to from) [ north+2*east,north+2*west,2*north+west,2*north+east,south+2*west,south+2*east,2*south+west,2*south+east ]
		Û -> concatMap (rec_move 1 from) diagonal
		Ü -> concatMap (rec_move 1 from) straight
		Ý -> concatMap (rec_move 1 from) (diagonal++straight)
		Þ -> concatMap (try_move_to from) (diagonal++straight) ++
			try_castle_kingside -- ++ try_castle_quenside
	piece_moves _ = []

	pawn_dir = if pColourToMove == White then north else south

	rec_move :: Int -> Coors -> (Int,Int) -> [Move]
	rec_move i from (f,r) = case try_move_to from (f*i,r*i) of
		moves@(Move _ to Nothing _ : _) -> moves ++ rec_move (i+1) from (f,r)
		moves -> moves

	try_move_to :: Coors -> (Int,Int) -> [Move]
	try_move_to from δ = maybe_move from ((Just from) +@ δ) ++ maybe_take from ((Just from) +@ δ)

	maybe_take from (Just to) = case pBoard!to of
		Just (col,_) | col /= pColourToMove -> [ Move from to (Just to) promo | promo <- promotions to ]
		_ -> []
	maybe_take _ _ = []
	maybe_move from (Just to) = case pBoard!to of
		Nothing -> [ Move from to Nothing promo | promo <- promotions to ]
		_ -> []
	maybe_move _ _ = []

	promotions to = case to of
		(_,Eighth) | pColourToMove==White -> map Just [Ú,Û,Ü,Ý]
		(_,First)  | pColourToMove==Black -> map Just [Ú,Û,Ü,Ý]
		_ -> [ Nothing ]

	try_castle_kingside = if
		pColourToMove `elem` pCanCastleKingSide &&
		isEmpty (F,base_row) && isEmpty (G,base_row) &&
		noCheck pos (E,base_row) && noCheck pos (F,base_row) && noCheck pos (G,base_row) then
		[ Move (E,base_row) (G,base_row) Nothing Nothing ] else []

	base_row = if pColourToMove == White then First else Eighth 
	isEmpty coors = isNothing (pBoard!coors)
	
	noCheck pos coors = not $ coors `elem` (map moveTo $ moveGen $ pos {
		pColourToMove = nextColour pColourToMove,
		pCanCastleQueenSide = [], pCanCastleKingSide = [] })

pb x = pBoard x

doMove Move{..} Position{..} = Position {
	pBoard				= pBoard // ( mb_take ++ move moveFrom moveTo ++ mb_castling ),
	pColourToMove       = nextColour pColourToMove,
	pCanCastleQueenSide = (if no_castling_queenside_any_more then delete pColourToMove else id) pCanCastleQueenSide,
	pCanCastleKingSide  = (if no_castling_kingside_any_more  then delete pColourToMove else id) pCanCastleKingSide,
	pEnPassantSquare    = Nothing,
	pHalfmoveClock      = if isJust moveTakes || moved_piece==Ù then 0 else pHalfmoveClock+1,
	pMoveCounter        = pMoveCounter + 1 }

	where

	move from to = [ (from,Nothing), (to,pBoard!from) ]

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

main = do
	loop [initialPosition]

loop poss@(pos:lastpos) = do
	print pos
	let moves = moveGen pos
	print moves
	putStr "> "
	s <- getLine
	case s of
		"q" -> return ()
		"b" -> loop lastpos
		m -> case filter ((m==).snd) (zip moves (map show moves)) of
			[] -> do
				putStrLn "No move."
				loop poss
			[(move,_)] -> loop (doMove move pos : poss)