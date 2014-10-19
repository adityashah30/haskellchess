module MoveModule where

import Data.List
import Data.Char
import Board

genMoveString :: Board->Pos->Pos->String
genMoveString b (x1, y1) (x2, y2)
        | isEmpty b (x1, y1) = [chr (y1 + ord 'a'), chr (ord '8' - x1)] ++ [chr (y2 + ord 'a'), chr (ord '8' - x2)]
        | isEmpty b (x2, y2) = [chr (y2 + ord 'a'), chr (ord '8' - x2)] ++ [chr (y1 + ord 'a'), chr (ord '8' - x1)]

genMovesString::BoardState -> BoardState -> String
genMovesString b1 b2 = 
    let b11 = concat $ snd b1
        b22 = concat $ snd b2
        z = zip b11 b22
        indices = findIndices (\(a,b)->a/=b) z
        p1 = indices!!0
        p2 = indices!!1
        pos1 = (p1 `div` 8, p1 `rem` 8)
        pos2 = (p2 `div` 8, p2 `rem` 8)
        m = if length indices == 2 then
            genMoveString (snd b2) pos1 pos2
        else
            case indices!!0 of
                0 -> "e1c1"
                4 -> "e1g1"
                56 ->"e8c8"
                _ -> "e8g8"
    in m

parseMove:: String -> (Pos,Pos)
parseMove "Not Found" = ((-1,-1) , (-1,-1))
parseMove str =  (pos1, pos2)
                    where
                        pos1 = getIndex ([str!!0] ++ [str!!1])
                        pos2 = getIndex ([str!!2] ++ [str!!3])

indexofMove::String -> Pos
indexofMove str = (a,b)
                where
                    b = (ord (str!!0) - ord 'a')
                    a = (7  - ord (str!!1) - ord '0' -1)

makeMove :: GameState->(Pos, Pos)->GameState
makeMove gs ((-1,-1) , (-1,-1)) = gs
makeMove gs (p1,p2) =
        let currBoardState = fst gs
            history = snd gs
            c = fst currBoardState
            b = snd currBoardState
            b' = movePiece b p1 p2
            c' = oppositeColor c
            gs' = ((c', b'), history++[currBoardState])
        in gs'

playGameUsingHistory::GameState -> [String] -> GameState
playGameUsingHistory gs  [] = gs
playGameUsingHistory gs  (move:moves) = playGameUsingHistory (makeMove gs (parseMove move)) moves