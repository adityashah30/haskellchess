module Evaluator where

import Board
import Pieces

-- The value of the king has to be infinity
-- the threshold has to be infinity minus
-- the maximal material value of one player
infinity = 1000::Int
threshold = 900::Int

--------Coefficients----------
coefficients = [10, 1, 1]::[Int]
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

--Given two vectors, calculate the dot product.
dotProduct::[Int] -> [Int] -> Int
dotProduct [] _ = 0
dotProduct _ [] = 0
dotProduct (x:xs) (y:ys) = x*y + dotProduct xs ys

--Given a board and a pieceOnSquare and a list of attacking positions, check if given pieceOnSqaure is safe.
checkIfSafe::Board -> PieceOnSquare -> [Pos] -> Bool
checkIfSafe bs _ [] = True
checkIfSafe bs posq (x:xs) =  if (getPieceOnSquare bs x) == posq then False else checkIfSafe bs posq xs

--Given different pieces, check if the given (Pos, PieceColor) is safe from them.
isSquareSafeFrom::Board -> (Pos, PieceColor) -> PieceType -> Bool
isSquareSafeFrom bs ((x, y), White) Pawn = and [validateBoundaryCondition (x-1, y-1), (getPieceOnSquare bs (x-1, y-1)) /= Just(Piece Pawn Black), validateBoundaryCondition (x-1, y+1), (getPieceOnSquare bs (x-1, y+1)) /= Just(Piece Pawn Black)]
isSquareSafeFrom bs ((x, y), Black) Pawn = and [validateBoundaryCondition (x+1, y-1), (getPieceOnSquare bs (x+1, y-1)) /= Just(Piece Pawn White), validateBoundaryCondition (x+1, y+1), (getPieceOnSquare bs (x+1, y+1)) /= Just(Piece Pawn White)]
isSquareSafeFrom bs ((x, y), pc) pt = checkIfSafe bs (Just (Piece pt (oppositeColor pc))) possiblePiecePos
    where 
        possiblePiecePos = genSemiValidMoves (updateBoard bs (x,y) (Just (Piece pt pc))) (x,y)

--Check if given (Pos, PieceColor) is safe from all of pieces.
isSquareSafe::Board -> (Pos, PieceColor) -> Bool
isSquareSafe bs ((x, y), pc) = and [pawn, rook, queen, knight, bishop]
    where 
        pawn = isSquareSafeFrom bs ((x, y), pc) Pawn
        rook = isSquareSafeFrom bs ((x, y), pc) Rook
        queen = isSquareSafeFrom bs ((x, y), pc) Queen
        knight = isSquareSafeFrom bs ((x, y), pc) Knight
        bishop = isSquareSafeFrom bs ((x, y), pc) Bishop

--Check of a given position lies on the diagonal.
isOnDiagonal::(Pos,PieceColor) -> Bool
isOnDiagonal ((pos1, pos2), _) = or [(pos1 == pos2), (pos1 == 7-pos2)]

--------------Central Square Analysis ----------------

--Check the central squares for piece occupation
checkCentralSquare::Board-> Pos -> (Int, Int)
checkCentralSquare b pos | getColor (getPieceOnSquare b pos) == Right Black = (0, 1)
                         | getColor (getPieceOnSquare b pos) == Right White = (1, 0)
                         | otherwise = (0, 0)


--Check the presence of pieces on central squares which are d4, d4, e4, e5
centralSquareAnalysis::Board->Int
centralSquareAnalysis b = p1-p2
                where
                    (p1, p2) = addTuples squareResults
                    squareResults = [d4, d5, e4, e5]
                    d4 = checkCentralSquare b (toPos "d4")
                    d5 = checkCentralSquare b (toPos "d5")
                    e4 = checkCentralSquare b (toPos "e4")
                    e5 = checkCentralSquare b (toPos "e5")

------Board Material Analysis-----------------------------------
boardMaterialAnalysis::Board->(Int,Int)
boardMaterialAnalysis b = foldl addValue (0,0) (concat b)
   where addValue points Nothing = points
         addValue (pw,pb) (Just (Piece a f)) | f == Black = (pw, pb + valueOfPiece a)
                                             | otherwise = (pw + valueOfPiece a, pb)

--Return the material strength of the board
evalBoardMaterial::Board->Int
evalBoardMaterial b = let (p1,p2) = boardMaterialAnalysis b in p1-p2

----------- Main Diagonal Analysis ----------------------------
boardDiagonalControlAnalysis::Board -> Int
boardDiagonalControlAnalysis b = pw - pb 
    where
        bishopPositions = foldl searchBishop (0,[]) (concat b)
        bishopsOnMainDiagonal = filter  isOnDiagonal (snd bishopPositions)
        pw =length ( filter (\(x,y) -> if y==White then True else False) safe)
        pb =length ( filter (\(x,y) -> if y==Black then True else False) safe)
        safe = safeDiagonal b bishopsOnMainDiagonal

--Given an (cell_num, [(Pos, PieceColor)]), return (cell_num, [(Pos, PieceColor)]) which conform to PieceOnSquare
searchBishop::(Int,[(Pos,PieceColor)])-> PieceOnSquare -> (Int,[(Pos,PieceColor)])
searchBishop (x , y) Nothing = ( x+1 , y)
searchBishop (x , y) (Just (Piece a f)) | a == Bishop = (x+1 , y++[((x `div` 8, x `rem` 8),f)])
                                        | otherwise = (x+1,y)

--Filter all safe diagonal positions among all the positions
safeDiagonal::Board -> [(Pos,PieceColor)] -> [(Pos,PieceColor)]
safeDiagonal b [] = []
safeDiagonal b (x: xs) = if (isSquareSafe b x) then ([x]++ (safeDiagonal b xs)) else (safeDiagonal b xs)

-----------Main Evaluation function-------------------------------------
applyCoeffs::[Int] -> Int
applyCoeffs features = dotProduct features coefficients

evalBoard::Board->Int
evalBoard b = applyCoeffs [(evalBoardMaterial b), (centralSquareAnalysis b), (boardDiagonalControlAnalysis b)]
