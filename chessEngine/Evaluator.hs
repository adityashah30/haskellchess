module Evaluator where

import Data.List
import Board
import Pieces
import MoveModule
import LearningModule

--------Evaluation Function Coefficients--------------
--In order: Material Strength, Central Squares, Diagonal Analysis, Connected Pawns, Checkmate.
coefficients = [10, 1, 1, 1, 10]::[Int]

--------Material Strength Coefficients--------
valueOfPiece::PieceType->Int
valueOfPiece Pawn = 1
valueOfPiece Rook = 5
valueOfPiece Knight = 3
valueOfPiece Bishop = 3
valueOfPiece Queen = 9
valueOfPiece King = infinity

--------Helper Functions---------------
--Add an array of tuples.
addTuples::[(Int, Int)] -> (Int, Int)
addTuples [] = (0, 0)
addTuples ((x,y):xs) = (x + fst a, y + snd a)
    where a = addTuples xs

--Given two vectors, calculate the dot product.
dotProduct::[Int] -> [Int] -> Int
dotProduct [] _ = 0
dotProduct _ [] = 0
dotProduct (x:xs) (y:ys) = (x*y) + (dotProduct xs ys)

--Check of a given position lies on the diagonal.
isOnDiagonal::(Pos) -> Bool
isOnDiagonal (pos1, pos2) = or [(pos1 == pos2), (pos1 == 7-pos2)]

--------------------- Connected Pawns ------------------
--Given an array of pawn positions and a score, if an adjacent pawn is present in a connected manner,
--then calculate further with the given score, else decrement the score and calculate further.
getPawnBreaks::[Pos] -> Int -> Int
getPawnBreaks [] num =  num
getPawnBreaks (x : []) num =  num
getPawnBreaks ((x1,x2):(y1,y2):xs) num = if((or [x1 == y1+1,x1==y1,x1==y1-1]) && (x2+1 == y2)) then (getPawnBreaks ((y1,y2):xs) num) else (getPawnBreaks ((y1,y2):xs) (num-1))

--Given a Board, calculate the number of groups of connected pawn. 0<=score<=7.
connectedPawnAnalysis::Board -> Int
connectedPawnAnalysis bs = (pw-pb)
    where
        whitePawnPos = sortBy compareByFile (getPiecePosInBoard bs (Piece Pawn White))
        blackPawnPos = sortBy compareByFile (getPiecePosInBoard bs (Piece Pawn Black))
        pw =  getPawnBreaks whitePawnPos 7
        pb =  getPawnBreaks blackPawnPos 7

--The compare function used by sortBy. Orders positions by file.
compareByFile::Pos -> Pos -> Ordering
compareByFile (_,x2) (_,y2) | x2<=y2 = LT
                            | otherwise = GT

--------------Central Square Analysis ----------------
--Check the central squares for piece occupation
checkCentralSquare::Board-> Pos -> (Int, Int)
checkCentralSquare b pos | getColor (getPieceOnSquare b pos) == Right Black = (0, 1)
                         | getColor (getPieceOnSquare b pos) == Right White = (1, 0)
                         | otherwise = (0, 0)

--Check the presence of pieces on central squares which are d4, d4, e4, e5
centralSquareAnalysis::Board->Int
centralSquareAnalysis b = (p1-p2)
                where
                    (p1, p2) = addTuples squareResults
                    squareResults = [d4, d5, e4, e5]
                    d4 = checkCentralSquare b (toPos "d4")
                    d5 = checkCentralSquare b (toPos "d5")
                    e4 = checkCentralSquare b (toPos "e4")
                    e5 = checkCentralSquare b (toPos "e5")

----------- Main Diagonal Analysis ----------------------------
diagonalControlAnalysis::Board -> Int
diagonalControlAnalysis b = (pw-pb)
    where      
        pw = length (safePosProtList b diagw White)
        pb = length (safePosProtList b diagb Black)
        diagw = filter isOnDiagonal (getPiecePosInBoard b (Piece Bishop White))
        diagb = filter isOnDiagonal (getPiecePosInBoard b (Piece Bishop Black))

---------------Dynamic Evaluation Scheme---------------------------
dynamicEvalBoard::Board -> Int
dynamicEvalBoard b = truncate$doubleVal
    where
        doubleVal = 10*(dynamicEval b loadKB)

------Board Material Analysis-----------------------------------
boardMaterialAnalysis::Board -> (Int,Int)
boardMaterialAnalysis b = foldl addValue (0,0) (concat b)
   where addValue points Nothing = points
         addValue (pw,pb) (Just (Piece a f)) | f == Black = (pw, pb + valueOfPiece a)
                                             | otherwise = (pw + valueOfPiece a, pb)

--Return the material strength of the board
evalBoardMaterial::Board->Int
evalBoardMaterial b = let (pw,pb) = boardMaterialAnalysis b in (pw-pb)

-----------Main Evaluation function-------------------------------------
applyCoeffs::[Int] -> Int
applyCoeffs features = dotProduct features coefficients

evalBoard::Board->Int
evalBoard b = applyCoeffs [(evalBoardMaterial b), (centralSquareAnalysis b), (diagonalControlAnalysis b), (connectedPawnAnalysis b), (checkMateEval b)]