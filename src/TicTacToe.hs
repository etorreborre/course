{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
module TicTacToe where

import Prelude
import Data.List

-- `move`: takes a tic-tac-toe board and position and moves to that position (if not occupied) returning a new board.
-- This function can only be called on a board that is empty or in-play. Calling `move` on a game board that is finished is a *compile-time type error*.

data Position =
 NE  | NC  | NW  |
 CE  | CC  | CW  |
 SE  | SC  | SW    deriving (Eq,Show)

data BoardCell = BoardCell Position Move deriving (Eq,Show)

data Cell = Empty | C Move deriving (Eq,Show)

data Move = X | O deriving (Eq,Show)

data Board a where
  Unfinished :: [BoardCell] -> Board UnfinishedBoard
  Finished   :: [BoardCell] -> Board FinishedBoard

cells :: forall a. Board a -> [BoardCell]
cells (Unfinished cs) = cs
cells (Finished cs)   = cs

-- Game state: unfinished, finished
data UnfinishedBoard
data FinishedBoard

data BoardTry = forall a. BoardTry (Board a) | Failed

instance Show BoardTry where
  show (BoardTry board) = show $ cells board
  show Failed           = show "Failed move"

move :: Board UnfinishedBoard -> Position -> Move -> BoardTry
move (Unfinished cells) p m =
  case find (contains p) cells of
    Just (BoardCell p1 m) -> Failed
    Nothing               -> if length cells == 9 then BoardTry $ Finished   ((BoardCell p m) : cells)
                             else                       BoardTry $ Unfinished ((BoardCell p m) : cells)

move' :: BoardTry -> Position -> Move -> BoardTry
move' (BoardTry (Unfinished cells)) p m = move (Unfinished cells) p m
move' _ _ _       = Failed

contains :: Position -> BoardCell -> Bool
contains p1 (BoardCell p2 _) = p1 == p2
