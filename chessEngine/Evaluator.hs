module Evaluator where

import Data.List
import Board
import Pieces

-- The value of the king has to be infinity
-- the threshold has to be infinity minus
-- the maximal material value of one player
infinity = 1000::Int
threshold = 900::Int

--------Coefficients----------
--In order: Material Strength, Central Squares, Diagonal Analysis, Connected Pawns
coefficients = [10, 1, 2, 3]::[Int]
------------------------------

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

secondOf::(a,b,c) -> b
secondOf (a, b, c) = b

--Given two vectors, calculate the dot product.
dotProduct::[Int] -> [Int] -> Int
dotProduct [] _ = 0
dotProduct _ [] = 0
dotProduct (x:xs) (y:ys) = (x*y) + (dotProduct xs ys)

--Check of a given position lies on the diagonal.
isOnDiagonal::(Pos) -> Bool
isOnDiagonal (pos1, pos2) = or [(pos1 == pos2), (pos1 == 7-pos2)]

--------------Generic System for checking attacking pieces-------
checkNumOfAttacks::Board -> PieceOnSquare -> [Pos] -> Int
checkNumOfAttacks bs _ [] = 0
checkNumOfAttacks bs posq (x:xs) = if(getPieceOnSquare bs x) == posq then (1+(checkNumOfAttacks bs posq xs)) else (checkNumOfAttacks bs posq xs)

isSquareAttackedBy::Board -> (Pos, PieceColor) -> PieceType -> Int
isSquareAttackedBy bs ((x, y), Black) Pawn = blacka + blackb
    where
        blacka = if and [validateBoundaryCondition (x-1, y-1), (getPieceOnSquare bs (x-1, y-1)) == Just(Piece Pawn Black)] then 1 else 0
        blackb = if and [validateBoundaryCondition (x-1, y+1), (getPieceOnSquare bs (x-1, y+1)) == Just(Piece Pawn Black)] then 1 else 0
isSquareAttackedBy bs ((x, y), White) Pawn = whitea + whiteb
    where
        whitea = if and [validateBoundaryCondition (x+1, y-1), (getPieceOnSquare bs (x+1, y-1)) == Just(Piece Pawn White)] then 1 else 0
        whiteb = if and [validateBoundaryCondition (x+1, y+1), (getPieceOnSquare bs (x+1, y+1)) == Just(Piece Pawn White)] then 1 else 0
isSquareAttackedBy bs ((x,y), pc) pt = checkNumOfAttacks bs (Just (Piece pt pc)) possiblePiecePos
    where
        possiblePiecePos = genSemiValidMoves (updateBoard bs (x,y) (Just (Piece pt (oppositeColor pc)))) (x,y)

calcNumAttacks::Board -> Pos -> [(Int, Int)]
calcNumAttacks bs pos = [p, b, k, r, q]
    where
        p = (isSquareAttackedBy bs (pos, White) Pawn, isSquareAttackedBy bs (pos, Black) Pawn)
        b = (isSquareAttackedBy bs (pos, White) Bishop, isSquareAttackedBy bs (pos, Black) Bishop)
        k = (isSquareAttackedBy bs (pos, White) Knight, isSquareAttackedBy bs (pos, Black) Knight)
        r = (isSquareAttackedBy bs (pos, White) Rook, isSquareAttackedBy bs (pos, Black) Rook)
        q = (isSquareAttackedBy bs (pos, White) Queen, isSquareAttackedBy bs (pos, Black) Queen)

--Check if a piece at pos (x,y) and piececolor pc is safe from oppsite colored pieces.
isSquareSafe::Board -> Pos -> PieceColor -> Bool
isSquareSafe bs pos White = if sum(map snd (calcNumAttacks bs pos)) == 0 then True else False
isSquareSafe bs pos Black = if sum(map fst (calcNumAttacks bs pos)) == 0 then True else False

--Check if a piece at pos (x,y) and piececolor pc is protected by pieces of color piececolor.
isSquareProtected::Board -> Pos -> PieceColor -> Bool
isSquareProtected bs pos White = if sum(map fst (calcNumAttacks bs pos)) > 0 then True else False
isSquareProtected bs pos Black = if sum(map snd (calcNumAttacks bs pos)) > 0 then True else False

--Filter all safe and Protected positions among all the positions
safePosList::Board -> [Pos] -> PieceColor -> [Pos]
safePosList _ [] _ = []
safePosList b (x:xs) pc = if ((isSquareSafe b x pc)||(isSquareProtected b x pc)) then ([x]++ (safePosList b xs pc)) else (safePosList b xs pc)

getPiecePosInBoard::Board-> Piece -> [Pos]
getPiecePosInBoard b p = secondOf (foldl searchPiece (0, [], p) (concat b))

searchPiece::(Int,[Pos],Piece)-> PieceOnSquare -> (Int,[Pos],Piece)
searchPiece (x, y, p) Nothing = (x+1, y, p)
searchPiece (x, y, p) (Just p1)    | p1==p = (x+1, y++[(x `div` 8, x `rem` 8)], p)
                                   | otherwise = (x+1, y, p)

--------------------- Connected Pawns ------------------

getPawnBreaks::[Pos] -> Int -> Int
getPawnBreaks [] num =  num
getPawnBreaks (x : []) num =  num
getPawnBreaks ((x1,x2):(y1,y2):xs) num = if((or [x1 == y1+1,x1==y1,x1==y1-1]) && (x2+1 == y2)) then (getPawnBreaks ((y1,y2):xs) num) else (getPawnBreaks ((y1,y2):xs) (num-1))

connectedPawnAnalysis::Board -> Int
connectedPawnAnalysis bs = (pw-pb)
    where
        whitePawnPos = sortBy compareByFile (getPiecePosInBoard bs (Piece Pawn White))
        blackPawnPos = sortBy compareByFile (getPiecePosInBoard bs (Piece Pawn Black))
        pw =  getPawnBreaks whitePawnPos 7
        pb =  getPawnBreaks blackPawnPos 7

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
        pw = length (safePosList b diagw White)
        pb = length (safePosList b diagb Black)
        diagw = filter isOnDiagonal (getPiecePosInBoard b (Piece Bishop White))
        diagb = filter isOnDiagonal (getPiecePosInBoard b (Piece Bishop Black))

------Board Material Analysis-----------------------------------
boardMaterialAnalysis::Board -> (Int,Int)
boardMaterialAnalysis b = foldl addValue (0,0) (concat b)
   where addValue points Nothing = points
         addValue (pw,pb) (Just (Piece a f)) | f == Black = (pw, pb + valueOfPiece a)
                                             | otherwise = (pw + valueOfPiece a, pb)

--Return the material strength of the board
evalBoardMaterial::Board->Int
evalBoardMaterial b = let (p1,p2) = boardMaterialAnalysis b in (p1-p2)

-----------Main Evaluation function-------------------------------------
applyCoeffs::[Int] -> Int
applyCoeffs features = dotProduct features coefficients

evalBoard::Board->Int
evalBoard b = applyCoeffs [(evalBoardMaterial b), (centralSquareAnalysis b), (diagonalControlAnalysis b), (connectedPawnAnalysis b)]