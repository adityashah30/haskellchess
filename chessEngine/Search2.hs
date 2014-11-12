module Search2 where

import Data.Tree.Game_tree.Negascout
import Data.Tree.Game_tree.Game_tree
import Board
import Pieces
import Evaluator
import OpenBookModule

data GameTree = GameTree {state::BoardState, gameState::GameState} deriving Eq

upD

instance Game_tree GameTree
    where
        is_terminal gt = (children gt == [])
        node_value (GameTree bs _) = evalBoard$snd$bs
        children (GameTree bs gs) = nextStatesAdvanced gs