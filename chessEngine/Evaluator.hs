module Evaluator where

import Board
import Pieces

-- The value of the king has to be infinity
-- the threshold has to be infinity minus
-- the maximal material value of one player
infinity = 100000::Int
threshold = 60500::Int

materialStrengthCoeff = 10::Int

valueOfPiece::PieceType->Int
valueOfPiece Pawn = 100
valueOfPiece Rook = 500
valueOfPiece Knight = 300
valueOfPiece Bishop = 325
valueOfPiece Queen = 900
valueOfPiece King = infinity

-- aggregated value of material of both players
boardMaterialAnalysis::Board->(Int,Int)
boardMaterialAnalysis b = foldl addValue (0,0) (concat b)
   where addValue points Nothing = points
         addValue (pw,pb) (Just (Piece a f)) | f == Black = (pw, pb + valueOfPiece a)
                                             | otherwise = (pw + valueOfPiece a, pb)

evalBoardMaterial::Board->Int
evalBoardMaterial b = let (p1,p2) = boardMaterialAnalysis b in p1-p2

applyCoeff::Int -> Int
applyCoeff f = materialStrengthCoeff*f

evalBoard::Board->Int
evalBoard b = applyCoeff$evalBoardMaterial$b
