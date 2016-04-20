{-# LANGUAGE RecordWildCards,UnicodeSyntax #-}

module View (
	module Text.Parsec.String,
	module View
	) where

import Text.Parsec hiding (Line)
import Text.Parsec.String
import Data.Char
import Data.Maybe
import Data.List
import Control.Monad
import System.IO
import System.CPUTime
import Text.Printf
import Data.Array
import Control.Monad.State.Strict

import Core


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
	movestr_p (str,move) = try (string str >> return move)
	move_p pos = choice $ map movestr_p $ zip (map (showMove pos) moves) moves
		where
		Right moves = moveGenerator pos 

testsuite_p = sepBy epd_p (skipMany (string " ") >> endOfLine)

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

putStrConsole s = liftIO $ do
	putStr s
	hFlush stdout
--	BSC.putStrLn $ BSU.fromString s
--	appendFile "test.txt" (s++"\n")

putStrConsoleLn s = putStrConsole $ s ++ "\n"

stringToPosition :: Colour -> [String] -> Position
stringToPosition col_to_move s = Position (array ((1,1),(8,8)) $
	zip [ (f,r) | r <- [8,7..1], f <- [1..8] ] (map tofig (concat s)))
	col_to_move [White,Black] [White,Black] Nothing 0 0
	where
	tofig c | c >= 'ç' = tofig (chr $ ord c - ord 'ç' + ord 'Ø')
	tofig 'Ø' = Nothing
	tofig c = Just (toEnum (div i 6),toEnum (mod i 6)) where
		i = ord c - ord 'Ù'

t = do
	f <- readFile "BK-Test.txt"
	let ls = lines f
	forM_ ls $ \ l -> do
		print $ parse epd_p "" l

showMove_FromTo Move{..} = showCoors moveFrom ++ showCoors moveTo ++ maybe "" pieceStr movePromote

showLine :: [Move] -> String
showLine moves = intercalate ", " (map showMove_FromTo moves)

showLinePretty :: Position -> [Move] -> String
showLinePretty _ [] = ""
showLinePretty pos (move:moves) = showMove pos move ++ ", " ++ showLinePretty (doMove pos move) moves

readMove pos movestr = head [ move | let Right moves = moveGenerator pos, move <- moves, movestr == showMove_FromTo move ]

showMovesPretty pos moves = intercalate ", " $ map (showMove pos) moves

showMovesInPos pos = do
	let Right moves = moveGenerator pos
	putStrLn $ showMovesPretty pos moves

showSearchState s = printf "Tot. %i Nodes, %i Leaves, %i Evals, alpha/beta-Cutoffs=%i/%i"
	(nodesProcessed s) (leavesProcessed s) (evaluationsDone s) (αCutoffs s) (βCutoffs s)

comp_progress _ [] = 0.0 :: Float
comp_progress ts ((i,n):ins) = product ts * fromIntegral i / fromIntegral n + comp_progress ((1 / fromIntegral n):ts) ins

