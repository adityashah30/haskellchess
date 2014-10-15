module Evaluator where

import Board
import Pieces

-- The value of the king has to be infinity
-- the threshold has to be infinity minus
-- the maximal material value of one player
infinity = 1000::Int
threshold = 900::Int

valueOfPiece::PieceType->Int
valueOfPiece Pawn = 1
valueOfPiece Rook = 5
valueOfPiece Knight = 3
valueOfPiece Bishop = 3
valueOfPiece Queen = 9
valueOfPiece King = infinity

-- aggregated value of material of both players
boardAnalysis::Board->(Int,Int)
boardAnalysis b = foldl addValue (0,0) (concat b)
   where addValue points Nothing = points
         addValue (pw,pb) (Just (Piece a f)) | f == Black = (pw, pb + valueOfPiece a)
                                             | otherwise = (pw + valueOfPiece a, pb)

evalBoard::Board->Int
evalBoard b = let (p1,p2) = boardAnalysis b in p1-p2

