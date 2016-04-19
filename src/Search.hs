module Search where

import Core
import View

data SearchState = SearchState {
	debugMode           :: Bool,
	nodesProcessed      :: Int,
	lastNodesProcessed  :: Int,
	leavesProcessed     :: Int,
	evaluationsDone     :: Int,
	lastStateOutputTime :: Integer,
	lastCPUTime         :: Integer,
	αCutoffs            :: Int,
	βCutoffs            :: Int,
	computationProgress :: [(Int,Int)],
	killerMoveHits      :: Int,
	memoizationHits     :: Int,
	memoizationMisses   :: Int,
	positionHashtable   :: HashMap.HashMap Position (Rating,Line),
	killerMoves         :: IntMap.IntMap [Move],
	principalVariation  :: Line,
	prevPositionHashtable :: Maybe (HashMap.HashMap Position (Rating,Line)) }
	deriving (Show)
initialSearchState = SearchState False 0 0 0 0 0 0 0 0 [] 0 0 0 HashMap.empty IntMap.empty [] Nothing

type SearchMonad a = StateT SearchState IO a

single_search depth position = do
	do_search depth 0 position [] (-mAX_RATING,mAX_RATING) True

aspirationWindow = True
aspirationRadius = 0.33
iterativeStartDepth = 4

iterative_deepening :: Depth -> Position -> SearchMonad (Rating,Line)
iterative_deepening maxdepth position = do
	putStrConsoleLn $ printf "\n========== iterative_deepening : maxdepth=%i" maxdepth
	putStrConsoleLn $ printf "intial search..."
	(rating,best_line) <- do_search iterativeStartDepth 0 position [] (-mAX_RATING,mAX_RATING) False
	let αβ = case aspirationWindow of
		False -> (-mAX_RATING,mAX_RATING)
		True  -> (rating-aspirationRadius,rating+aspirationRadius)
	iter (iterativeStartDepth+2) (rating,best_line) αβ
	where
	iter :: Depth -> (Rating,Line) -> AlphaBetaWindow -> SearchMonad (Rating,Line)
	iter m_depth last_res _ | m_depth > maxdepth = return last_res
	iter m_depth (last_rating,last_best_line) (α,β) = do
		putStrConsoleLn $ printf "\n========== iter : m_depth=%i,  maxdepth=%i" m_depth maxdepth
		putStrConsoleLn $ printf "== alpha/beta=(%+.2f,%+.2f),  principal_var: %s\n" α β (showLine last_best_line)
		modify' $ \ s -> initialSearchState {
			principalVariation = last_best_line,
			killerMoves = killerMoves s,
			debugMode   = debugMode s,
			prevPositionHashtable = Just (positionHashtable s) }
		res@(rating,best_line) <- do_search m_depth 0 position [] (α,β) True
		putStrConsoleLn $ printf "\n\nFound %+.2f : %s" rating (showLinePretty position best_line)
		case (rating<α,rating>β) of
			(True,False) -> do
				putStrConsoleLn $ printf "rating=%+.2f < α=%+.2f => FAIL LOW, expanding and re-searching...\n" rating α
				iter m_depth res (rating-aspirationRadius,β)
			(False,True) -> do
				putStrConsoleLn $ printf "rating=%+.2f > β=%+.2f => FAIL HIGH, expanding and re-searching...\n" rating β
				iter m_depth res (α,rating+aspirationRadius)
			_ -> iter (m_depth+2) res (rating-aspirationRadius,rating+aspirationRadius)

numKillerMovesPerPly = 3
memoizationPlies = [1,2,3,4,5]

type AlphaBetaWindow = (Rating,Rating)
type Line = [Move]

do_search :: Depth -> Depth -> Position -> Line -> AlphaBetaWindow -> Bool -> SearchMonad (Rating,Line)
do_search maxdepth depth position current_line (α,β) pv_node = do
--	putStrConsoleLn $ printf "----- CURRENT LINE: %s " (showLine current_line)
	case moveGenerator position of
		Left  _  -> return (evalPosition position,current_line)
		Right unsorted_moves -> do
			hashmap <- gets positionHashtable
			case HashMap.lookup position hashmap of
				Just res -> do
					modify' $ \ s -> s { memoizationHits = memoizationHits s + 1 }
					return res
				Nothing -> do
					modify' $ \ s -> s { memoizationMisses = memoizationMisses s + 1 }
					s <- get
					let
						principal_moves = case pv_node of
							True  -> take 1 $ drop depth (principalVariation s)
							False -> []
						killer_moves = IntMap.findWithDefault [] depth (killerMoves s)
						presorted_moves = (if maximizing then reverse else id) $ case prevPositionHashtable s of
							Nothing -> sortBy (comparing move_sort_val) unsorted_moves
							Just ht -> map fst $ sortBy (comparing snd) $ map prev_val unsorted_moves where
								prev_val m = (m,case HashMap.lookup (doMove position m) ht of
									Just (v,_) -> v
									Nothing    -> 0.0 )
						moves = ((principal_moves ++ killer_moves) `intersect` presorted_moves) ++
							((presorted_moves \\ principal_moves) \\ killer_moves)
					modify' $ \ s -> s { computationProgress = (0,length moves) : computationProgress s }
					res <- find_best_line (worst_val,undefined) moves pv_node
					when (depth `elem` memoizationPlies) $ do
						modify' $ \ s -> s { positionHashtable = HashMap.insert position res (positionHashtable s) }
					modify' $ \ s -> s { computationProgress = tail (computationProgress s) }
					return res

		where

		move_sort_val (Move from to mb_takes mb_promote) =
			maybe 0 (\ c -> max 1 (piecetypeval_at c - piecetypeval_at from)) mb_takes +
			maybe 0 (const 10) mb_promote
			where
			piecetypeval_at coors = let Just (_,piecetype) = (positionBoard position)!coors in 1 + index (Ù,Þ) piecetype

		maximizing = positionColourToMove position == White

		(worst_val,isBetterThan) = if maximizing then (-mAX_RATING,(>)) else (mAX_RATING,(<))

		find_best_line :: (Rating,[Move]) -> [Move] -> Bool -> SearchMonad (Rating,Line)
		find_best_line best [] _ = return best
		find_best_line best@(best_val,best_line) (move:moves) pv_node = do
			let
				position' = doMove position move
				current_line' = current_line ++ [move]
				depth' = depth + 1
			modify' $ \ s -> s { nodesProcessed = nodesProcessed s + 1 }

			(val,subline) <- case depth' < maxdepth of
				True -> do
					let
						full_αβ = if maximizing then (best_val,β) else (α,best_val)
						null_window = if maximizing then (α,α+0.000001) else (β-0.000001,β)
					case pv_node of
						True  -> do_search maxdepth depth' position' current_line' full_αβ pv_node
						False -> do
							res'@(val',_) <- do_search maxdepth depth' position' current_line' null_window pv_node
							case α < val' && val' < β of
								False -> return res'
								True  -> do_search maxdepth depth' position' current_line' full_αβ pv_node
				False -> do
					modify' $ \ s -> s {
						leavesProcessed = leavesProcessed s + 1,
						nodesProcessed  = nodesProcessed  s + 1,
						evaluationsDone = evaluationsDone s + 1 }
					last_output_secs <- gets lastStateOutputTime
					TOD current_secs _ <- liftIO getClockTime
					when (current_secs - last_output_secs >=1) $ do
						SearchState{..} <- get
						current_cpu_time <- liftIO getCPUTime
						let nodes_per_sec = div (1000000 * fromIntegral $ nodesProcessed - lastNodesProcessed)
							(max (div (current_cpu_time - lastCPUTime) 1000000) 1)
						putStrConsole $ printf "[%3.0f%%] %i_N %4i_N/s Cuts:%i/%i KilHits:%i Memo(Pos:%i Hits:%i Miss:%i)    \r"
							(100.0 * comp_progress [] (reverse computationProgress))
							nodesProcessed nodes_per_sec αCutoffs βCutoffs killerMoveHits (HashMap.size positionHashtable)
							memoizationHits memoizationMisses
						modify' $ \ s -> s {
							lastStateOutputTime = current_secs,
							lastNodesProcessed = nodesProcessed,
							lastCPUTime = current_cpu_time }
					return (evalPosition position',current_line')		

			modify' $ \ s -> s { computationProgress = let ((i,n):ps) = computationProgress s in (i+1,n):ps }

			best' <- case val `isBetterThan` best_val of
				True  -> return (val,subline)
				False -> return best

			case (maximizing && val >= β) || (not maximizing && val <= α) of
				True  -> do  -- CUTOFF
					modify' $ \ s -> case maximizing of
						True  -> s { βCutoffs = βCutoffs s + 1 }
						False -> s { αCutoffs = αCutoffs s + 1 }
					modify' $ \ s -> case move `elem` (IntMap.findWithDefault [] depth (killerMoves s)) of
						True  -> s { killerMoveHits = killerMoveHits s + 1 }
						False -> s { killerMoves = IntMap.insertWith
							(\ new old -> take numKillerMovesPerPly $ new ++ old)
							depth [move] (killerMoves s) }
					return best'
				False -> find_best_line best' moves False
