{-# LANGUAGE TupleSections,TypeSynonymInstances,FlexibleInstances,ScopedTypeVariables,RecordWildCards,FlexibleContexts,UnicodeSyntax,DeriveGeneric #-}

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

-- is the square coors not threatened by next player
no_check pos coors = all (\ (_,(_,(to,_))) -> coors /= to) $
	move_targets (pos { positionColourToMove = nextColour (positionColourToMove pos) })

-- the position of the king of the player to move in this position
kings_coors Position{..} = head $ [ coors | (coors,Just (col,Þ)) <- assocs positionBoard, col==positionColourToMove ]

-- would the king of the player to move in pos be in check after the move
king_no_check pos move = no_check pos' (kings_coors pos') where
	pos' = (doMove pos move) { positionColourToMove = positionColourToMove pos }

