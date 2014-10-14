module TicTacToe where

-- `move`: takes a tic-tac-toe board and position and moves to that position (if not occupied) returning a new board. This function can only be called on a board that is empty or in-play. Calling `move` on a game board that is finished is a *compile-time type error*.

data Position =
 NE  | NC  | NW  |
 CE  | CC  | CW  |
 SE  | SC  | SW

data BoardCell = BoardCell Position Move

data Cell = Empty | C Move

data Move = X | O

data Board =
	Cells [BoardCell]

data NotFull = NotFull
data Full = Full

move :: Board -> Position -> Move -> Board
move b c m = undefined

undefined = undefined
