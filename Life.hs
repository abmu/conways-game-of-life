import Data.Set (Set)

type Cell = (Int, Int)

type Board = Set Cell

neighbours :: Cell -> [Cell]
neighbours (x,y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0)]