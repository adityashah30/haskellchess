module OpenBookModule where

import System.Random
import System.IO.Unsafe
import Board
import MoveModule
import FileModule

type OpeningBook = [(Int, Int, String, String)]

--Every move represented by a 4-tuple (ID, Parent, White, Black).
--Black king side castling: e8g8
--Black queen side castling: e8c8
--White king side castling: e1g1
--White queen side castling: e1c1

--The filepath for openBookTree.
openBookTreePath::FilePath
openBookTreePath = "../data/open_book_tree.dat"

--Function to load the opening book tree given filepath.
loadMovesArray::FilePath -> OpeningBook
loadMovesArray fpath = read (unsafePerformIO $ loadFromFile fpath)::OpeningBook

--Abstraction to load opening book tree.
movesArray::OpeningBook
movesArray = loadMovesArray openBookTreePath

--Extract the parent's ID from a node.
getParentID::(Int, Int, String, String) -> Int
getParentID (_, parent, _, _) = parent

--Extract the ID from a node.
getID::(Int, Int, String, String) -> Int
getID (id_, _, _, _) = id_

--Extract the black move from a node.
getBlackMove::(Int, Int, String, String) -> String
getBlackMove (_, _, _, black) = black

--Extract the white move from a node.
getWhiteMove::(Int, Int, String, String) -> String
getWhiteMove (_, _, white, _) = white

--Extratc both white and black moves in that order.
getBothMoves::(Int, Int, String, String) -> (String, String)
getBothMoves x = (getWhiteMove x, getBlackMove x)

--Given ID number of the node and the movesArray, extract the node
getNodeFromArray::Int -> OpeningBook -> (Int, Int, String, String)
getNodeFromArray _ [] = (-1, -1, "NULL", "NULL")
getNodeFromArray id_ (x:xs) = if (getID x) == id_ then x else (getNodeFromArray id_ xs)

-- Given the ID number of the node, extracts the node from the tree.
getNode::Int -> OpeningBook -> (Int, Int, String, String)
getNode id_ openingBook = getNodeFromArray id_ openingBook

--Given an array and the parent's ID, extracts a list of children IDs.
getChildrenFromArray::Int -> OpeningBook -> [Int]
getChildrenFromArray _ [] = []
getChildrenFromArray pid (x:xs) = if (getParentID x) == pid then [(getID x)]++(getChildrenFromArray pid xs) else (getChildrenFromArray pid xs)

--The wrapper function for getChildrenFromArrat to abstract the loading of the opening tree.
getChildren::Int -> OpeningBook -> [Int]
getChildren pid openingBook= getChildrenFromArray pid openingBook

--Given a list of IDs and a move, matches the move to ID and returns the ID of the matching move.
matchChild::[Int] -> (String, String) -> OpeningBook-> Int
matchChild [] _ _= -2
matchChild (x:xs) (white, black) openingBook = if (and [(getWhiteMove$(getNode x openingBook)) == white, (getBlackMove$(getNode x openingBook)) == black]) then (getID$(getNode x openingBook)) else (matchChild xs (white, black) openingBook)

--Takes a parent ID and the move sequence and matches the sequence to yield the final list of children
matchSequence::Int -> [(String, String)] -> OpeningBook -> [Int]
matchSequence (-2) _ _= [-2]
matchSequence pid [] openingBook = getChildren pid openingBook
matchSequence pid (x:xs) openingBook = matchSequence (matchChild (getChildren pid openingBook) x openingBook) xs openingBook

--Given a list, randomly select one element.
selectRandomMove::[Int] -> Int
selectRandomMove [] = -1
selectRandomMove xs = unsafePerformIO $ fmap (xs!!) $ randomRIO (0, length xs - 1)

--Given possible candidates for nextMove, choose those whose white move matches lastMove.
matchLastMove::[Int] -> String -> OpeningBook -> [Int]
matchLastMove [] _ _= []
matchLastMove (x:xs) lastMove openingBook= if (getWhiteMove$(getNode x openingBook)) == lastMove then [x]++(matchLastMove xs lastMove openingBook) else (matchLastMove xs lastMove openingBook)

--Given a movesString, traverse the tree and get the next node. Used for openings.
getNextEvenChild::[(String, String)] -> OpeningBook -> (String, String)
getNextEvenChild moves openingBook = getBothMoves$(getNode (selectRandomMove$(matchSequence (-1) moves openingBook)) openingBook)

--Given a movesString and white move, traverse the tree and get the next node. Used for defence.
getNextOddChild::([(String, String)], String) -> OpeningBook -> (String, String)
getNextOddChild (moves, lastMove) openingBook = getBothMoves$(getNode (selectRandomMove$(matchLastMove (matchSequence (-1) moves openingBook) lastMove openingBook)) openingBook)

--Given a movesString convert it to a list of (W,B) moves. Used for opening.
convertToEvenTuple::[String] -> [(String, String)]
convertToEvenTuple [] = []
convertToEvenTuple (x:[]) = []
convertToEvenTuple (x:y:z) = [(x, y)] ++ convertToEvenTuple z

--Given a movesString convert it to a tuple of list of (W,B) and the last move. Used for defence.
convertToOddTuple::[String] -> ([(String, String)], String)
convertToOddTuple [] = ([], "")
convertToOddTuple x = (convertToEvenTuple x, last x)

--Given a movesString, get the next move to be played.
getNextMove::[String] -> OpeningBook -> String
getNextMove moves openingBook= case (length moves `mod` 2) of 
                        0 -> fst$getNextEvenChild (convertToEvenTuple$moves) openingBook
                        1 -> snd$getNextOddChild (convertToOddTuple$moves) openingBook

--Given a gamestate, generate the movesString.
historyParser::GameState -> [String]
historyParser (_, []) = []
historyParser (currBoard, x:[]) = [genMovesString x currBoard]
historyParser (b, (x:y:xs)) = [(genMovesString x y)] ++ historyParser (b, [y]++xs)

--Given a gamestate, return the next gamestate.
getStateOpenBook:: GameState -> OpeningBook -> GameState
getStateOpenBook gs openingBook =  makeMove gs (parseMove $ getNextMove (historyParser gs) openingBook)
