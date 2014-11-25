module MoveModule where

import Data.List
import Data.List.Split
import Data.Char
import Board
import Pieces

--Given two Board configurations and from and to positions, generate the moveString.
genMoveString :: Board->Board->Pos->Pos->String
genMoveString b1 b2 (x1, y1) (x2, y2)
        | isEmpty b2 (x1, y1) = if (isPromotionPossible b1 b2 (x1,y1) (x2,y2)) then mstring1++"q" else mstring1
        | isEmpty b2 (x2, y2) = if (isPromotionPossible b1 b2 (x2,y2) (x1,y1)) then mstring2++"q" else mstring2
        where mstring1 = [chr (y1 + ord 'a'), chr (ord '8' - x1)] ++ [chr (y2 + ord 'a'), chr (ord '8' - x2)]
              mstring2 = [chr (y2 + ord 'a'), chr (ord '8' - x2)] ++ [chr (y1 + ord 'a'), chr (ord '8' - x1)]

--Old foo to generate moveString. Doesn't consider pawn promotion.
genMoveStringOld :: Board->Pos->Pos->String
genMoveStringOld b (x1, y1) (x2, y2)
        | isEmpty b (x1, y1) = [chr (y1 + ord 'a'), chr (ord '8' - x1)] ++ [chr (y2 + ord 'a'), chr (ord '8' - x2)]
        | isEmpty b (x2, y2) = [chr (y2 + ord 'a'), chr (ord '8' - x2)] ++ [chr (y1 + ord 'a'), chr (ord '8' - x1)]

--Given two boardstates, generate the movesString using genMoveString.
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
        p3 = indices!!2
        pos3 = (p3 `div` 8, p3 `rem` 8)
        to = getEnpassantToMove [pos1, pos2, pos3]
        from = getEnpassantFromMove to [pos1, pos2, pos3]
        m = if length indices == 2 then
            genMoveString (snd b1) (snd b2) pos1 pos2
        else if length indices == 3 then
            genMoveString (snd b1) (snd b2) to from
            --genMoveStringOld (snd b2) pos1 pos2
        else
            case indices!!0 of
                0 -> "e1c1"
                4 -> "e1g1"
                56 ->"e8c8"
                _ -> "e8g8"
    in m

--Given a list of moves, determine the enpassant to move Pos.
getEnpassantToMove::[(Int, Int)] -> (Int, Int)
getEnpassantToMove [] = (-1,-1)
getEnpassantToMove ((x,y):xs) = if or [x==2, x==5] then (x,y) else getEnpassantToMove xs

--Given a list of moves, determine the enpassant from move Pos.
getEnpassantFromMove::(Int, Int) -> [(Int, Int)] -> (Int, Int)
getEnpassantFromMove to [] = (-1,-1)
getEnpassantFromMove to ((x,y):xs) = if y/=snd(to) then (x,y) else getEnpassantFromMove to xs

--Convert moveString to tuple of positions (from, to)
parseMove:: String -> (Pos,Pos)
parseMove "NULL" = ((-1,-1) , (-1,-1))
parseMove str =  (pos1, pos2)
                    where
                        pos1 = getIndex ([str!!0] ++ [str!!1])
                        pos2 = getIndex ([str!!2] ++ [str!!3])

--Convert rank and file to row and column,
getIndex::String -> Pos
getIndex str = (ord '8' - ord (str!!1), ord (str!!0) - ord 'a')


indexofMove::String -> Pos
indexofMove str = (a,b)
                where
                    b = (ord (str!!0) - ord 'a')
                    a = (7  - ord (str!!1) - ord '0' -1)

--Given a gamestate, make the move and update history and currBoard.
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

--Update gamestate using movesString.
playGameUsingHistory::GameState -> [String] -> GameState
playGameUsingHistory gs  [] = gs
playGameUsingHistory gs  (move:moves) = playGameUsingHistory (makeMove gs (parseMove move)) moves

--Given a gamestate, return all the list of boards except the initialBoard.
getBoardsFromGameState::GameState -> [Board]
getBoardsFromGameState (bs, history) = [snd$bs] ++ (map snd (init history))

--Given a board, get a list of all possible next moves.
getNextMoves::Board -> [(Piece, Pos, Pos)]
getNextMoves b = (getNextColorMoves b White) ++ (getNextColorMoves b Black)

--Given a board and color, generate all possible next moves for that color.
getNextColorMoves::Board -> PieceColor -> [(Piece, Pos, Pos)]
getNextColorMoves b pc = map (\(x, y, z) -> (getPiece x, y, z)) (filter (\(x, _, xs) -> if or[x==Nothing, getColor x == Right (oppositeColor pc)] then False else True) (concat$map (getNextMoveTuple b movesList) [(i,j) | i<-[0..7], j<-[0..7]]))
            where
                movesList = genValidMoves b

--Given a board and a movesList(a list of moves for every cell on the board), and a position, return
--a list of all the possible moves for that position.
getNextMoveTuple::Board -> [[[Pos]]] -> Pos -> [(PieceOnSquare, Pos, Pos)]
getNextMoveTuple b movesList (x,y) = getNextMovesTuple(getPieceOnSquare b (x,y), (x,y), movesList!!x!!y)

--Given a piece, pos and a list of moves, expand the moves to generate (piece, frompos, topos) where to pos
--belongs to the list of moves.
--e.g (Piece Pawn Black, (1,0) [(2,0),(3,0)]) becomes [(Piece Pawn Black, (1,0), (2,0), (Piece Pawn Black, (1,0), (3,0)]
getNextMovesTuple::(PieceOnSquare, Pos, [Pos]) -> [(PieceOnSquare, Pos, Pos)]
getNextMovesTuple (p, pos, []) = []
getNextMovesTuple (p, pos, (x:xs)) = [(p, pos, x)] ++ (getNextMovesTuple (p, pos, xs))

--Given a moveString in xboard format, convert it to a list of strings
--e.g "d2d4 e7e5 e2e4" becomes ["d2d4", "e7e5", "e2e4"]
getMoveStringList::String -> [String]
getMoveStringList [] = []
getMoveStringList moves = splitOn " " moves