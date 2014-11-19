module Search2 where

import Data.Tree.Game_tree.Game_tree
import Data.Tree.Game_tree.Negascout
import Board
import Pieces
import Evaluator
import OpenBookModule

data TreeNode = TreeNode {state::GameState} deriving (Eq, Show)

instance Game_tree TreeNode
    where
        is_terminal (TreeNode gs) = or [children (TreeNode gs) == [],finalBoardState$fst$gs]
        node_value (TreeNode gs) = evalGameState gs
        children (TreeNode gs) = createChildren (TreeNode gs)

treeDepth::Int
treeDepth = 4

evalGameState::GameState -> Int
evalGameState ((White, b), _) = evalBoard b
evalGameState ((Black, b), _) = -(evalBoard b)

evalBoardState::BoardState -> Int
evalBoardState s = evalBoard (snd s)

createChildren::TreeNode->[TreeNode]
createChildren (TreeNode (s, h))
    | finalBoardState s = []
    | otherwise = map (\x -> (TreeNode x)) nextGameStates
    where nextGameStates = map (\bs->(bs, s:h)) (nextStatesAdvanced (s, h))

finalBoardState::BoardState->Bool
finalBoardState st = sw > threshold || sw < -threshold
   where sw = evalBoardState st

getSecondFromList::[TreeNode] -> TreeNode
getSecondFromList list = head$tail$list

getBoardStateFromPV::([TreeNode], Int) -> BoardState
getBoardStateFromPV (x, _) = fst$state$getSecondFromList x

getNextState2::GameState -> OpeningBook -> BoardState
getNextState2 gs openingBook =
                if newGs == gs then
                    getBoardStateFromPV (alpha_beta_search (TreeNode gs) treeDepth)
                else
                    (fst newGs)
            where
                newGs = getStateOpenBook gs openingBook

displayPV::[TreeNode] -> IO()
displayPV [] = return ()
displayPV ((TreeNode gs):xs) = do{
                                putStrLn "--------";
                                displayBoard$snd$fst$gs;
                                putStrLn "--------";
                                displayPV xs;
                                }