module Search2 where

-- ToDO: Create all game tree for 
--
-- position fen 1r2k3/2q5/8/8/8/K7/8/8 w - - 0 1 moves a3a4
--
-- Check for which of the nodes is children == [] but isCheckMate == False

import Data.Tree.Game_tree.Game_tree
import Data.Tree.Game_tree.Negascout
import Board
import Pieces
import Evaluator
import LearningModule
import OpenBookModule

data TreeNode = TreeNode {state::GameState, color::PieceColor} deriving (Eq, Show)

instance Game_tree TreeNode
    where
        is_terminal (TreeNode gs pc) = isCheckMate$snd$fst$gs
        node_value (TreeNode gs pc) = evalGameState gs pc
        children (TreeNode gs pc) = createChildren gs pc

treeDepth::GameState->Int
treeDepth gs 
        | numOpponentPieces <= 3 = 4
        | otherwise = 4
        where numOpponentPieces = length(positionsWithThisColor (oppositeColor$fst$fst$gs) (snd$fst$gs))

--evalGameState::GameState -> PieceColor -> Int
--evalGameState gs Black = evalBoard$snd$fst$gs
--evalGameState gs White = -(evalBoard$snd$fst$gs)

evalGameState::GameState -> PieceColor -> Int
evalGameState gs _ = (evalBoard$snd$fst$gs)

createChildren::GameState -> PieceColor ->[TreeNode]
createChildren (s, h) pc = map (\bs-> (TreeNode (bs, s:h) pc)) (nextStatesAdvanced (s, h))

getSecondFromList::[TreeNode] -> TreeNode
getSecondFromList list = head$tail$list

getBoardStateFromPV::([TreeNode], Int) -> BoardState
getBoardStateFromPV (x, _) = fst$state$getSecondFromList x

getNextState2::GameState -> OpeningBook -> BoardState
getNextState2 gs openingBook =
                if newGs == gs then
                    getBoardStateFromPV (negascout (TreeNode gs (fst$fst$gs)) (treeDepth gs))
                else
                    (fst newGs)
            where
                newGs = getStateOpenBook gs openingBook

displayPV::[TreeNode] -> IO()
displayPV [] = return ()
displayPV ((TreeNode gs _):xs) = do{
                                putStrLn "--------";
                                displayBoard$snd$fst$gs;
                                putStrLn "--------";
                                displayPV xs;
                                }                                

evalBoardStates::[BoardState]->[(Board, Int)]
evalBoardStates [] = []
evalBoardStates (x:xs) = [(snd$x, evalBoard$snd$x)]++(evalBoardStates xs)

displayBoards::[(Board, Int)] -> IO()
displayBoards [] = return ()
displayBoards ((b,val):xs) = do{
                            displayBoard b;
                            putStrLn "-----";
                            print( show(val));
                            putStrLn "-----";
                            displayBoards xs;
                        }

displayTreeNode::[TreeNode] -> IO()
displayTreeNode [] = return ()
displayTreeNode ((TreeNode gs pc):xs) = do{
                                        displayBoard$snd$fst$gs;
                                        print("Value: "++show(evalGameState gs pc));
                                        displayTreeNode xs;
                                    }

printGameTree::TreeNode -> Int -> Int -> IO()
printGameTree root md d
                    | d == (md+1)= return ()
                    | otherwise = do{
                                    displayTreeNode [root];
                                    print("Depth: "++show(d));
                                    print("Is terminal: "++show(is_terminal root));
                                    print("Is children == []: "++show((children root) == []));
                                    putStrLn "-----------------------------";
                                    printGameTree' ch md (d+1);
                                    }
                                    where ch = children root

printGameTree'::[TreeNode] -> Int -> Int -> IO()
printGameTree' [] _ _ = return ()
printGameTree' (x:xs) md d = do{
                                printGameTree x md d;
                                printGameTree' xs md d
                            }