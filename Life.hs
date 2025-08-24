import qualified Data.Set as S
import Data.Set (Set)

type Cell = (Int, Int)

type Board = Set Cell

neighbours :: Cell -> [Cell]
neighbours (x,y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0)]

liveNeighbours :: Board -> Cell -> Int
liveNeighbours b c = length $ filter (`S.member` b) (neighbours c)

-- Candidates for the next generation are every live cell, and their neighbours
candidates :: Board -> Set Cell
candidates b = S.unions $ map (\c -> S.fromList (c : neighbours c)) (S.toList b)