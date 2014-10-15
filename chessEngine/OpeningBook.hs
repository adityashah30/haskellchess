module OpeningBook where

import System.Random
import System.IO.Unsafe
import FileModule

--Every move represented by a 4-tuple (ID, Parent, Black, White).
--This Opening book represents 18 variations of the Queen's Gambit
--Black king side castling: e8g8
--Black queen side castling: e8c8
--White king side castling: e1g1
--White queen side castling: e1c1

loadMovesArray::FilePath -> [(Int, Int, String, String)]
loadMovesArray fpath = read (unsafePerformIO $ loadFromFile fpath)::[(Int, Int, String, String)]

parseMoveArray::String -> [(Int, Int, String, String)]
parseMoveArray string = read string::[(Int, Int, String, String)]

getParent::(Int, Int, String, String) -> Int
getParent (_, parent, _, _) = parent

getID::(Int, Int, String, String) -> Int
getID (id_, _, _, _) = id_

getBlack::(Int, Int, String, String) -> String
getBlack (_, _, black, _) = black

getWhite::(Int, Int, String, String) -> String
getWhite (_, _, _, white) = white

getQueenGambitChildren::Int -> [(String, String)]
getQueenGambitChildren parent = getChildren parent (loadMovesArray "../data/opening_book_tree.dat")

getChildren::Int -> [(Int, Int, String, String)]-> [(String, String)]
getChildren parent [] = []
getChildren parent (x:xs) = if(parent == (getParent x)) then (getBlack x, getWhite x):(getChildren parent xs)
else  (getChildren parent xs)

checkIfEqual::Int -> (String, String) -> (Int, Int, String, String) -> Bool
checkIfEqual parent (black, white) (_, parent1, b, w) = and [(parent == parent1), (black==b), (white==w)]

getQueenGambitID::Int -> (String, String) -> Int
getQueenGambitID parent move = getMoveID parent move (loadMovesArray "../data/opening_book_tree.dat")

getMoveID::Int -> (String, String) -> [(Int, Int, String, String)] -> Int
getMoveID _ _ [] = -1
getMoveID parent (black, white) moves = if checkIfEqual parent (black, white) (head(moves)) then getID (head(moves)) else getMoveID parent (black, white) (tail(moves))

getMove::String->[(String, String)]->[String]
getMove _ [] = ["Not Found"]
getMove black ((b,w):xs) = if black==b then w:(getMove black xs) else getMove black xs

selectMove::[String]->String
selectMove [] = "Not Found"
selectMove ["Not Found"] = "Not Found"
selectMove xs = unsafePerformIO $ fmap (xs!!) $ randomRIO (0, length xs - 1)

parseSequence::[String] -> [(String, String)]
parseSequence (b:[]) = []
parseSequence (b:w:[]) = []
parseSequence (b:w:m) = (b,w):(parseSequence m)

searchLastElement::Int->[(String, String)]->Int
searchLastElement parent [] = -1
searchLastElement parent (x:[]) = getQueenGambitID parent x
searchLastElement parent (x:xs) = searchLastElement (getQueenGambitID parent x) xs

searchBook::[String] -> String
searchBook [] = getWhite ((loadMovesArray "../data/opening_book_tree.dat")!!0)
searchBook seq_ = selectMove $ getMove (last seq_) (getQueenGambitChildren (searchLastElement (-1) (parseSequence ("NULL":seq_) )))
