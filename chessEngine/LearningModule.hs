module LearningModule where

import System.IO.Unsafe
import Data.List.Split
import ParserModule
import Board
import Pieces
import FileModule
import MoveModule

--Record is a 4-tuple with (Board Bi, count of Bi in KB, count of games won featuring Bi as White,count of games won featuring Bi as Black , [Next positions of Bi])
type Record = (Board, Double, Double, Double, Double)
--KB is (Number of games played, number of boards in KB(can be nonunique))
type KB = (Double, Double, [Record])

-------Board Functions------
--Used for king's posiition
infinity = 1000::Int
--Used for stalemate conditions
staleMateVal = 2000::Int
--Used for checkMate consitions
checkMateVal = 2000::Int

-------------- CheckMate ------------------------
getInBetweenPos::Pos -> Pos -> PieceType-> [Pos]
getInBetweenPos p1 p2 Pawn = [p2]
getInBetweenPos p1 p2 Knight = [p2]
getInBetweenPos (x1, y1) (x2, y2) Rook = if (x1==x2) then 
                                            if (y1<y2) then 
                                                [(x1, y) | y<-[(y1+1)..y2]] 
                                            else [(x1, y) | y<-[(y2+1)..y1]]
                                        else 
                                            if (y1==y2) then
                                                if (x1<x2) then
                                                    [(x, y1) | x <- [(x1+1)..x2]]
                                                else
                                                    [(x, y1) | x <- [(x2+1)..x1]]
                                            else
                                                []
getInBetweenPos (x1, y1) (x2, y2) Bishop = if (abs(x1-x2) == abs(y1-y2)) then
                                                if and [x1<x2, y1<y2] then
                                                    (zip [(x1+1)..x2] [(y1+1)..y2])
                                                else if and [x1<x2, y1>y2] then
                                                    (zip [(x1+1)..x2] (map abs [(1-y1)..(-y2)]))
                                                else if and [x1>x2, y1<y2] then
                                                    (zip (map abs [(1-x1)..(-x2)]) [(y1+1)..y2])
                                                else
                                                    (zip (map abs [(1-x1)..(-x2)]) (map abs [(1-y1)..(-y2)]))
                                            else
                                                []
getInBetweenPos (x1, y1) (x2, y2) Queen = (getInBetweenPos (x1, y1) (x2, y2) Rook) ++ (getInBetweenPos (x1, y1) (x2, y2) Bishop)

--isCheckMate::Board -> Bool
--isCheckMate b = if (checkMateEval b) == 0 then False else True

--checkMateEval::Board -> Int
--checkMateEval b = pw - pb
--            where
--                pw = if (null nextBlackMoveList) then staleMateVal else 
--                        if (null$otherPosList Black) then 0 else
--                            if (null blackKingPosList) then checkMateVal else
--                                if((length$otherPosList Black) /= 1) then checkMateVal else
--                                    if(null$filter (isSquareSafe b Black) (getInBetweenPos blackKingPos (otherPos Black) (pieceType$getPiece$getPieceOnSquare b (otherPos Black)))) then checkMateVal else 0
--                pb = if (null nextWhiteMoveList) then staleMateVal else 
--                        if (null$otherPosList White) then 0 else
--                            if(null whiteKingPosList) then checkMateVal else
--                                if((length$otherPosList White) /= 1) then checkMateVal else
--                                    if(null$filter (isSquareSafe b White) (getInBetweenPos whiteKingPos (otherPos White) (pieceType$getPiece$getPieceOnSquare b (otherPos White)))) then checkMateVal else 0
--                nextWhiteMoveList = getNextColorMoves b White
--                nextBlackMoveList = getNextColorMoves b Black
--                whiteKingPosList = getPiecePosInBoard b (Piece King White)
--                blackKingPosList = getPiecePosInBoard b (Piece King Black)
--                whiteKingPos = whiteKingPosList!!0
--                blackKingPos = blackKingPosList!!0
--                otherPosList pc = validateKingCheck b pc
--                otherPos pc = (otherPosList pc)!!0 
isCheckMate'::Board -> PieceColor -> Bool
isCheckMate' b pc = and [null (getNextColorMoves b pc), not$null (validateKingCheck b pc)]

isStaleMate'::Board -> PieceColor -> Bool
isStaleMate' b pc = and [null (getNextColorMoves b pc), null (validateKingCheck b pc)]

isCheckMate::Board -> Bool
isCheckMate b = or [isCheckMate' b White, isCheckMate' b Black]

isStaleMate::Board -> Bool
isStaleMate b = or [isStaleMate' b White, isStaleMate' b Black]

checkMateEval::Board -> Int
checkMateEval b = if (isCheckMate' b White) then (-checkMateVal)
                    else if (isCheckMate' b Black) then checkMateVal
                        else if (isStaleMate' b White) then staleMateVal
                            else if (isStaleMate' b Black) then (-staleMateVal)
                                else 0
-------------------------------------------------------------------------------
-----Helper Functions----------
--Given two vectors, calculate the dot product.
dotProductD::[Double] -> [Double] -> Double
dotProductD [] _ = 0
dotProductD _ [] = 0
dotProductD (x:xs) (y:ys) = (x*y) + (dotProductD xs ys)

--Given a KB and a board, return the record corresponding to the board if it exists else return a new record.
searchBoard::KB -> Board -> Record
searchBoard kb bs = if(null existingRecords) then initRecord bs else head$existingRecords
        where
            existingRecords = filter (\(b, _, _, _, _)-> if bs==b then True else False) (getRecords kb)

--Given a KB and a gamestate, add that information to the KB 
updateKB::KB -> GameState -> KB
updateKB oldKB gs = incrementGames newKB
    where
        a = getBoardsFromGameState gs
        newKB = foldl (updateKBBoard winningColor) oldKB a
        incrementGames (g, b, rlist) = (g+1, b, rlist)
        winningColor = if (winnerGame gs White) then White else Black

--Given a gamestate and piececolor determine whether that piececolor won the game.
winnerGame::GameState -> PieceColor -> Bool
winnerGame gs White = if ((checkMateEval$snd$fst$gs) >= infinity) then True else False
winnerGame gs Black = if ((checkMateEval$snd$fst$gs) <= -infinity) then True else False

--Given a piececolor and record, update the information about winning and return back the updated record.
updateRecords::PieceColor -> Record -> Record
updateRecords White (b, nbi, wwgbi, wbgbi, nbbi) = (b, nbi+1, wwgbi+1, wbgbi, nbbi)
updateRecords Black (b, nbi, wwgbi, wbgbi, nbbi) = (b, nbi+1, wwgbi, wbgbi+1, nbbi)

--Given a piececolor, KB and a board, update the relevant information and return back the updated KB.
updateKBBoard::PieceColor -> KB -> Board -> KB
updateKBBoard pc (g, b, rlist) brd = (g, b+1, newRecordList)
    where
        searchedRecord = searchBoard (g, b, rlist) brd
        newRecord = updateRecords pc searchedRecord
        newRecordList = updatedRecords rlist newRecord

--Given a list of records and a record, return back a list of records where the given record
-- was inserted in the record list if it didn't exist, else it was modified.
updatedRecords::[Record] -> Record -> [Record]
updatedRecords [] record = [record]
updatedRecords (x:xs) record = if((getBoard x) == (getBoard record)) then [record]++xs else [x]++(updatedRecords xs record)
--------------------------------
-----Init Functions-----------
--Create a trivial KB.
initKB::KB
initKB = (0, 0, [])

--Create a new record for a board
initRecord::Board -> Record
initRecord b = (b, 0, 0, 0, fromIntegral$length$getNextMoves b)
-----------------------------

--------Load functions--------
--The Filepath for the KB
kBPath::FilePath
kBPath = "../data/kb.dat"

--Loads the KB given the filepath.
loadKBFromFile::FilePath -> KB
loadKBFromFile fpath = read (unsafePerformIO $ loadFromFile fpath)::KB

--Abstraction for loading the KB from the filepath from kBPath.
loadKB::KB
loadKB = loadKBFromFile kBPath
-------------------------------
---------Store Functions-------
--Given a filepath and KB, write to to the file.
storeKB'::FilePath -> KB -> IO()
storeKB' fpath kb = writeToFile fpath (show kb)

--Abstraction to store the KB at the filepath from kBPath
storeKB::KB -> IO()
storeKB kb = storeKB' kBPath kb
------------------------------
----- Modify Functions -------
--Given a string(from the lastgame) modify the KB and store it back.
modifyKB:: String -> IO()
modifyKB [] = return ()
modifyKB lastMove = if(isCheckMate$snd$fst$lastGameState) then (modifyKB' lastGameState) else return ()
    where
        splitLines = splitOn "\n" lastMove
        lastIntergameState= case parseCommand (splitLines!!0) of
            Nothing -> initialGameState
            Just (CmdPosition gs) -> gs
            _ -> initialGameState
        lastGameState = case parseLastMoveString (splitLines!!1) of
            Just (movePos) -> makeMove lastIntergameState movePos
            _ -> lastIntergameState

--Helper function for modifyKB.
modifyKB'::GameState -> IO()
modifyKB' gs = storeKB newKB
    where 
        newKB = updateKB loadKB gs

--Given a KB, get the number of Games.
getNumGames::KB -> Double
getNumGames (g, _, _) = g

--Given a KB, get the number of Boards.
getNumBoards::KB -> Double
getNumBoards (_, b, _) = b

--Given a KB, get the list of records.
getRecords::KB -> [Record]
getRecords (_, _, rl) = rl

--Given a record, extract the board.
getBoard::Record -> Board
getBoard (b, _, _, _, _) = b

--Given a record, extract the count of board in the KB.
getNBi::Record -> Double
getNBi (_, nbi, _, _, _) = nbi

--Given a record, extract the count of white games won that contained this board.
getWWGBi::Record -> Double
getWWGBi (_, _, wwgbi, _, _) = wwgbi

--Given a record, extract the count of black games won that contained this board.
getWBGBi::Record -> Double
getWBGBi (_, _, _, wbgbi, _) = wbgbi

--Given a record, extract the count of white-black games won that contained this board.
getWGBi::Record -> Double
getWGBi r = (getWWGBi r) - (getWBGBi r)

--Given a record, extract the number of next moves from a board.
getNumNextBoardsBi::Record -> Double
getNumNextBoardsBi (_, _, _, _, nbbi) = nbbi

--Given the number of games g and list of records [x], calculate the list [wgbi(x)/g].
probWinBi::Double -> [Record] -> [Double]
probWinBi _ [] = []
probWinBi g (x:xs) = [(getWGBi x)/g] ++ (probWinBi g xs)

--Given the number of total boards and list of records [x], calculate the list [nbi(x)/b].
probBi::Double -> [Record] -> [Double]
probBi _ [] = []
probBi b (x:xs) = [(getNBi x)/b] ++ (probBi b xs)

--Given a board B and list of records [x], for each board b in [x] find the sim(B, b).
--Similarity between 2 boards B and b is described as 2*|common_next_moves(B, b|/(|next_moves(B)|+|next_moves(b)|).
sim::Board -> [Record] -> [Double]
sim _ [] = []
sim b (x:xs) = [(2*bbi)/(b1+bi)] ++ (sim b xs)
            where
                nb1 = getNextMoves b
                b1 = fromIntegral$length$nb1
                nbi = getNextMoves (getBoard x)
                bi = getNumNextBoardsBi x
                nbbi = [x| x <- nb1 , y <- nbi, x == y]
                bbi = fromIntegral$length$nbbi

--Given lists of probWinBi and sim, perform the dot product to get the probability of winning and board B.
probWinB::Board -> KB -> Double
probWinB b kb = dotProductD (probWinBi (getNumGames kb) (getRecords kb)) (sim b (getRecords kb))

--Given the lists of probBi and sim, perform dot product to get the probability of board B.
probB::Board -> KB -> Double
probB b kb = dotProductD (probBi (getNumBoards kb) (getRecords kb)) (sim b (getRecords kb))

--Given values of probWinB and probB, find the probability of winning given board B.
dynamicEval::Board -> KB -> Double
dynamicEval b kb = (probWinB b kb)/(probB b kb)