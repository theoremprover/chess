{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

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

import Data.Stack
import System.IO
import Text.Printf

import DGTSerial
import Chess200

main = loop 2 initialPosition stackNew where
	loop :: Depth → Position → Stack Position → IO ()
	loop maxdepth pos pos_history = do
		print pos
		putStrLn $ printf "Rating = %.2f" (fst $ rate pos)
		putStrLn $ "Possible moves are:" ++ show (moveGen pos)
		case rate pos of
			(_,Just matchresult) → print matchresult
			_ | otherwise        → return ()
		input <- putStr "? " >> hFlush stdout >> getLine
		case input of
			"i" → loop maxdepth initialPosition stackNew
			"q" → return ()
			"s" → execute_move $ last $ snd $ search maxdepth pos []
			"b" → case stackPop pos_history of
				Nothing → do
					putStrLn "There is no previous position."
					loop maxdepth pos pos_history
				Just (stack',prev_pos) → loop maxdepth prev_pos stack'
			depth_str | [(depth,[])] <- reads depth_str → loop depth pos pos_history
			move_str → case lookup move_str $ map (\ m → (show m,m)) (moveGen pos) of
				Nothing   → do
					putStrLn "This is no (legal) move or command."
					loop maxdepth pos pos_history
				Just move → execute_move move
		where
		execute_move move = do
			putStrLn $ "Moving " ++ show move
			loop maxdepth (doMove pos move) (stackPush pos_history pos)
