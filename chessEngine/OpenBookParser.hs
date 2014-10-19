module OpenBookParser where

import Data.List
import Data.Char
import OpeningBook
import Board
import MoveModule

historyParser::GameState -> [String]
historyParser (_, []) = []
historyParser (currBoard, x:[]) = [genMovesString x currBoard]
historyParser (b, (x:y:xs)) = [(genMovesString x y)] ++ historyParser (b, [y]++xs)

getStateOpenBook:: GameState -> GameState
getStateOpenBook gs =  makeMove gs (parseMove $ searchBook $ historyParser gs)