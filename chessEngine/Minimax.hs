module Minimax where

import Board
import Pieces
import Evaluator

data GameTree = GameTree {state::BoardState, children::[GameTree]}

-- maximal depth of the tree
depth::Int
depth = 3

prettyGameTree::GameTree->String
prettyGameTree = prettyGameTree2 0
   where prettyGameTree2 x (GameTree z bs) = prettyBoardIndent (10*x) (snd z) ++ 
                                               ' ':show (evalState z) ++ 
                                               concatMap (prettyGameTree2 (x+1)) bs

evalState::BoardState->Int
evalState s = evalBoard (snd s)

genGameTree::Int->GameState->GameTree
genGameTree 0 (s, _) = GameTree s []
genGameTree maxdepth (s, h)
		| finalState s = GameTree s []
		| otherwise = GameTree s (map (genGameTree (maxdepth-1)) nextGameStates)
		where	nextGameStates = map (\bs->(bs, s:h)) (nextStatesAdvanced (s, h))

-- minmax algorithm, computes value of best outcome
minmax::GameTree->Int
minmax (GameTree p []) = evalState p
minmax (GameTree (White,_) xs) = maximum (map minmax xs)
minmax (GameTree (Black,_) xs) = minimum (map minmax xs)

getNextState::GameState->BoardState
getNextState gs = case (genGameTree depth gs) of
                  GameTree p [] -> p
                  GameTree (f, _) xs -> snd (findBestNextState f (compare f) (map (\x->(minmax x, state x)) xs))
    where compare White = (>)
          compare Black = (<)

findBestNextState :: PieceColor -> (Int -> Int -> Bool) -> [(Int, BoardState)] -> (Int, BoardState)
findBestNextState _ _ [x] = x
findBestNextState f cmp ((x1,y1):xs) 
			| winningState f y1 = (x1,y1)
			| otherwise = let (x2, y2) = findBestNextState f cmp xs in
                                             if cmp x1 x2 then (x1,y1) else (x2,y2)
                                             

finalState::BoardState->Bool
finalState st = sw > threshold || sw < -threshold
   where sw = evalState st

winningState::PieceColor->BoardState->Bool
winningState White st = evalState st > threshold
winningState Black st = evalState st < -threshold

-- *************************************************************************************

playGame::GameState->Int->GameState
playGame currGameState counter
	| (counter == 1) = ((getNextState currGameState), updatedHistory)
	| otherwise = playGame ((getNextState currGameState), updatedHistory) (counter-1)
	where	currState = fst currGameState
		history = snd currGameState
		updatedHistory = history ++ [currState]
		
initializeGame::GameState->GameState
initializeGame game =
	let	counter = 20
	in playGame game counter
	
-- *************************************************************************************
	
displayGame::GameState->IO ()
displayGame (currState, []) = do	displayBoard (snd currState)
					putStrLn "DONE"
displayGame (currState, (h:hs)) = do	displayBoard (snd h)
					putStrLn "_________________________________"
					putStrLn ""
					displayGame (currState, hs)

-- *************************************************************************************

