module Search where

import Board
import Pieces
import Evaluator
import MoveModule
import OpenBookModule

data GameTree = GameTree {state::BoardState, children::[GameTree]}

-- maximal (treeDepth gs) of the tree
treeDepth::GameState->Int
treeDepth gs 
        | numOpponentPieces <= 3 = 4
        | otherwise = 4
        where numOpponentPieces = length(positionsWithThisColor (oppositeColor$fst$fst$gs) (snd$fst$gs))

alpha::Int
alpha = -40000

beta::Int
beta = 40000

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
		| finalState s = GameTree s []  --(genGameTree (maxdepth-1) (s, s:h))]
		| otherwise = GameTree s (map (genGameTree (maxdepth-1)) nextGameStates)
		where	nextGameStates = map (\bs->(bs, s:h)) (nextStatesAdvanced (s, h))


-- minmax algorithm, computes value of best outcome
minmax::GameTree->Int
minmax (GameTree p []) = evalState p
minmax (GameTree (White,_) xs) = (maximum (map minmax xs)) `div` 2
minmax (GameTree (Black,_) xs) = (minimum (map minmax xs)) `div` 2


alphabeta::GameTree->Int->Int->Int
alphabeta (GameTree p []) _ _ = evalState p
alphabeta (GameTree (White, board) [x]) a b = alphabeta x a b
alphabeta (GameTree (White, board) (x:xs)) a b
				| val >= b = val
				| otherwise = max val (alphabeta (GameTree (White, board) xs) a' b)
				where	val = alphabeta x a b
					a' = max val a
alphabeta (GameTree (Black, board) [x]) a b = alphabeta x a b
alphabeta (GameTree (Black, board) (x:xs)) a b
				| val <= a = val
				| otherwise = min val (alphabeta (GameTree (Black, board) xs) a b')
				where	val = alphabeta x a b
					b' = min val b


flag:: Bool
flag = True

getNextState::GameState->OpeningBook->BoardState
getNextState gs openingBook = 
                if newGs == gs then 
                  case (genGameTree (treeDepth gs) gs) of
                  GameTree p [] -> p
                  GameTree (f, _) xs -> snd (findBestNextState f (compare f) (map (\x->(minmax x, state x)) xs))
                  --GameTree (f, _) xs -> snd (findBestNextState f (compare f) (map (\x->(alphabeta x alpha beta, state x)) xs))
                  else
                    (fst newGs)
    where newGs = getStateOpenBook gs openingBook
          compare White = (>)
          compare Black = (<)

findBestNextState :: PieceColor -> (Int -> Int -> Bool) -> [(Int, BoardState)] -> (Int, BoardState)
findBestNextState _ _ [x] = x
findBestNextState f cmp ((x1,y1):xs) 
			| winningState f y1 = (x1,y1)
			| otherwise = let (x2, y2) = findBestNextState f cmp xs in
                                             if cmp x1 x2 then (x1,y1) else (x2,y2)
                                             

finalState::BoardState->Bool
finalState (pc,b) = (null (getNextColorMoves b pc)) 

winningState::PieceColor->BoardState->Bool
winningState pc bs = (null (getNextColorMoves (snd bs) pc))


-- *************************************************************************************

playGame::GameState->Int->OpeningBook->GameState
playGame currGameState counter openingBook
	| (counter == 1) = ((getNextState currGameState openingBook), updatedHistory)
	| otherwise = playGame ((getNextState currGameState openingBook), updatedHistory) (counter-1) openingBook
	where	currState = fst currGameState
		history = snd currGameState
		updatedHistory = history ++ [currState]
		
initializeGame::GameState->OpeningBook->GameState
initializeGame game openingBook=
	let	counter = 20
	in playGame game counter openingBook
	
-- *************************************************************************************
	
displayGame::GameState->IO ()
displayGame (currState, []) = do	displayBoard (snd currState)
					putStrLn "DONE"
displayGame (currState, (h:hs)) = do	displayBoard (snd h)
					putStrLn "_________________________________"
					putStrLn ""
					displayGame (currState, hs)

-- *************************************************************************************

