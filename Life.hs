module Main where

import qualified Data.Set as S
import Data.Set (Set)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

type Cell = (Int, Int)
type Board = Set Cell

data World = World { board :: Board, paused :: Bool }

neighbours :: Cell -> [Cell]
neighbours (x, y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0,0)]

liveNeighbours :: Board -> Cell -> Int
liveNeighbours b c = length $ filter (`S.member` b) (neighbours c)

-- Candidates for the next generation are every live cell, and their neighbours
candidates :: Board -> Set Cell
candidates b = S.unions $ map (\c -> S.fromList (c : neighbours c)) (S.toList b)

-- Conway's step
step :: Board -> Board
step b = S.fromList [c | c <- S.toList (candidates b), let n = liveNeighbours b c, (S.member c b && (n == 2 || n == 3)) || (not (S.member c b) && (n == 3))]

updateWorld :: Float -> World -> World
updateWorld _ w
    | paused w = w
    | otherwise = w { board = step (board w) }

-- Convert board to gloss pictures
cellSize :: Float
cellSize = 32

cellToPicture :: Cell -> Picture
cellToPicture (x, y) = translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) $ color white $ rectangleSolid cellSize cellSize

boardToPicture :: Board -> Picture
boardToPicture b = pictures $ map cellToPicture (S.toList b)

-- Convert screen pos to cell pos on board
screenToCell :: (Float, Float) -> Cell
screenToCell (x, y) = (round (x / cellSize), round (y / cellSize))

toggleCell :: Cell -> Board -> Board
toggleCell c b
    | S.member c b = S.delete c b
    | otherwise = S.insert c b

-- Handle input
handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) w = w { paused = not (paused w) }
handleEvent (EventKey (MouseButton LeftButton) Down _ pos) w
    | paused w = w { board = toggleCell (screenToCell pos) (board w) }
handleEvent _ w = w

-- Test patterns
glider :: Board
glider = S.fromList [(1,0),(2,1),(0,2),(1,2),(2,2)]

-- Main
main :: IO ()
main = do
    let initialWorld = World { board = glider, paused = False }
    play
        (InWindow "Conway's Game of Life" (1024, 1024) (-1, -1))
        black
        5
        initialWorld
        (boardToPicture . board)
        handleEvent
        updateWorld
