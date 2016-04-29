{-# LANGUAGE UnicodeSyntax #-}

{- Compiling:
ghc -O2 -rtsopts -fforce-recomp --make Main.hs
Main.exe +RTS -s -H1G
-}

{- Profilng:
ghc -fforce-recomp -prof -fprof-auto -O2 --make Main.hs
Main.exe +RTS -h -p
hp2ps -c Main.hp
-}

module Main where

{-
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
-}
import Text.Printf
import Data.Char
import Data.List

import Core
import Search
import View
import Polyglot

testPosition = stringToPosition Black [
	"çØçØóâçÜ",
	"ØîØçØçØç",
	"çØçØçØçØ",
	"ÙçØçØçØç",
	"çØçØçØçØ",
	"ØçØçØçØç",
	"çØçØçØçØ",
	"ÜçØçÞçØç" ]

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
				(c:[]) | c `elem` "si" -> do
					let search_fun = case c of
						's' -> single_search
						'i' -> iterative_deepening
					putStrConsoleLn "Searching..."
					((val,line),s) <- runStateT (search_fun depth pos) initialSearchState
					case line of
						[] -> putStrConsoleLn $ printf "Value = %+2.2f, no move possible." val
						best_move:_ -> do
							putStrConsoleLn $ "\n\n" ++ showSearchState s
							putStrConsoleLn $ "Found best line: " ++ showLinePretty pos line
							do_move best_move
				"b" -> return ()
				"q" -> error $ "Quit."
				"t" -> runTestSuite depth
				"r" -> loop depth initialPosition
				"book" -> do
					book <- openingBook
					forM_ (openingBookProposals book pos) $ \ (move,weight) -> do
						putStrConsoleLn $ printf "  -> %5s ( %i )" (showMove pos move) weight
					loop depth pos
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
	res <- parseFromFile testsuite_p "BK-Test.txt"
	case res of
		Left err -> error $ show err
		Right tests -> do
			res <- forM (drop 1 tests) $ \ (name,pos,best_moves) -> do
				putStrConsoleLn $ printf "\n###############################################"
				putStrConsoleLn $ printf "# Test %s\n" name
				showPos pos
				(_,best_line@(bm:_)) <- evalStateT (iterative_deepening depth pos) initialSearchState
				putStrConsoleLn $ printf "\n\nFound    best line  %s" (showLinePretty pos best_line)
				putStrConsoleLn $ printf "Expected best moves %s" (showMovesPretty pos best_moves)
				verdict <- case bm `elem` best_moves of
					False -> do
						putStrConsoleLn $ printf "=> FAIL !"
						return False
					True -> do
						putStrConsoleLn $ printf "=> ok."
						return True
				return (name,pos,bm,best_moves,verdict)

			forM_ res $ \ (name,pos,bm,best_moves,verdict) -> do
				putStrConsoleLn $ printf "Test %-7s: Expected %-5s, Found %-5s => %s" name (showMove pos bm)
					(showLinePretty pos best_moves) (if verdict then "ok" else "FAIL")
