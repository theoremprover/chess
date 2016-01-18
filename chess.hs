module Main where

import Data.Array

data File = A | B | C | D | E | F | G | H
	deriving (Show,Eq,Ord,Ix)
type Rank = Int

type Coors = (File,Rank)

data Colour = White | Black
	deriving (Eq,Show)

nextColour White = Black
nextColour Black = White

data Piece = Pawn | Bishop | Knight | Rook | Queen | King
	deriving (Show,Eq)

type Board = Array Coors (Maybe (Colour,Piece))

initialSetup = listArray ((A,1),(H,8)) $ map topiece [
	_,_,_,_,♔,_,_,___,
	___,___,___,___,___,___,___,___,
	___,___,___,___,___,___,___,___,
	___,___,___,___,___,___,___,___,
	___,___,___,___,___,___,___,___,
	___,___,___,___,___,___,___,___,
	___,___,___,___,___,___,___,___,
	___,___,___,___,___,___,___,___ ]
	where
	_ = Nothing
	w f = Just (White,topiece f)
	topiece 