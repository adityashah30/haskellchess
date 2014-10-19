module Board where
import Pieces
import Data.Char
import System.Random

type PieceOnSquare = Maybe Piece

type Board = [[PieceOnSquare]]

type Pos = (Int, Int)

-- PieceColor indicates whose turn it is
type BoardState = (PieceColor, Board)

type History = [BoardState]

type GameState = (BoardState, History)

-- ***************** board output ********************

displayBoard::Board->IO ()
displayBoard board = putStrLn ((unlines (map (concatMap displayPieceOnSquare) board)))

prettyBoardIndent::Int->Board->String
prettyBoardIndent x = ('\n':) . concatMap ((('\n':take x (repeat ' '))++) . concatMap displayPieceOnSquare)

instance Show PieceColor where
 show Black = "B"
 show White = "W"

instance Show PieceType where
 show King = "K"
 show Queen = "Q"
 show Knight = "N"
 show Rook = "R"
 show Bishop = "B"
 show Pawn = "P"
 
{-instance Show Piece where
 show (Piece King Black) = "k"
 show (Piece Queen Black) = "q"
 show (Piece Knight Black) = "n"
 show (Piece Rook Black) = "r"
 show (Piece Bishop Black) = "b"
 show (Piece Pawn Black) = "p"
 show (Piece King White) = "K"
 show (Piece Queen White) = "Q"
 show (Piece Knight White) = "N"
 show (Piece Rook White) = "R"
 show (Piece Bishop White) = "B"
 show (Piece Pawn White) = "P"-}
 

displayPieceOnSquare::PieceOnSquare->String
displayPieceOnSquare Nothing = "-- "
displayPieceOnSquare (Just (Piece a f)) = show f ++ show a ++ " "

-- *************** helper functions *******************

replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls


-- **************** board functions *******************

isEmpty::Board->Pos->Bool
isEmpty board pos = Nothing == getPieceOnSquare board pos

isNotEmpty::Board->Pos->Bool
isNotEmpty board pos = (not)(isEmpty board pos)

emptySquare::PieceOnSquare
emptySquare = Nothing

oppositeColor::PieceColor->PieceColor
oppositeColor White = Black
oppositeColor Black = White

getPieceOnSquare::Board->Pos->PieceOnSquare
getPieceOnSquare board (a, b)  = board!!a!!b

getPiece::PieceOnSquare->Piece
getPiece (Just (Piece x c)) = Piece x c

getColor::PieceOnSquare->Either Bool PieceColor
getColor Nothing = Left False
getColor sqr = Right (pieceColor (getPiece sqr))

hasOppColor::PieceOnSquare->PieceOnSquare->Bool
hasOppColor sqr1 sqr2 = (getColor sqr1) /= (getColor sqr2)

hasSameColor::PieceOnSquare->PieceOnSquare->Bool
hasSameColor sqr1 sqr2 = (getColor sqr1) == (getColor sqr2)

validateSquareForKnight::Board->PieceOnSquare->Pos->Bool
validateSquareForKnight board sqr pos = (isEmpty board pos)||(hasOppColor sqr (getPieceOnSquare board pos))


-- computes the representation of "a1:h8"

toPos::String->Pos
toPos [x, y] = (7 - (ord y - ord '1'), ord x - ord 'a')

-- **************** generating empty board moves for each piecetype ****************
-- Positions the piece can move to, on an empty board, given the piece's current position

genEmptyBoardMoves::Piece->Pos->[Pos]

genEmptyBoardMoves (Piece Rook _) (x,y) = rightHorizontalMoves (x,y) ++ leftHorizontalMoves (x,y) ++ downVerticalMoves (x,y) ++ upVerticalMoves (x,y)

genEmptyBoardMoves (Piece Bishop _) (x,y) = bottomRightDiagonalMoves (x,y) ++ topLeftDiagonalMoves (x,y) ++ bottomLeftDiagonalMoves (x,y) ++ topRightDiagonalMoves (x,y)

genEmptyBoardMoves (Piece Queen _) (x,y) = rightHorizontalMoves (x,y) ++ leftHorizontalMoves (x,y) ++ downVerticalMoves (x,y) ++ upVerticalMoves (x,y) ++ bottomRightDiagonalMoves (x,y) ++ topLeftDiagonalMoves (x,y) ++ bottomLeftDiagonalMoves (x,y) ++ topRightDiagonalMoves (x,y)

genEmptyBoardMoves (Piece King _) (x,y) = filter (validateBoundaryCondition) [(x,y+1),(x,y-1),(x+1,y),(x-1,y),(x+1,y+1),(x-1,y-1),(x+1,y-1),(x-1,y+1)]

genEmptyBoardMoves (Piece Knight _) (x,y) = filter (validateBoundaryCondition) [(x+2,y+1),(x+2,y-1),(x-2,y+1),(x-2,y-1),(x+1,y+2),(x-1,y+2),(x+1,y-2),(x-1,y-2)]

genEmptyBoardMoves (Piece Pawn White) (x,y) = filter (validateBoundaryCondition) ([(x-1,y),(x-1,y+1),(x-1,y-1)] ++ [(x-2,y) | (x==6)])

genEmptyBoardMoves (Piece Pawn Black) (x,y) = filter (validateBoundaryCondition) ([(x+1,y),(x+1,y+1),(x+1,y-1)] ++ [(x+2,y) | (x==1)])

-- ******************* movement iterating functions ***********************

rightHorizontalMoves::Pos->[Pos]
rightHorizontalMoves (x,y) = [ (i,j) | i<-[x],j<-[(y+1)..7] ]

leftHorizontalMoves::Pos->[Pos]
leftHorizontalMoves (x,y) = [ (i,j) | i<-[x],j<-[(y-1),(y-2)..0] ]

downVerticalMoves::Pos->[Pos]
downVerticalMoves (x,y) = [ (i,j) | i<-[(x+1)..7],j<-[y] ]

upVerticalMoves::Pos->[Pos]
upVerticalMoves (x,y) = [ (i,j) | i<-[(x-1),(x-2)..0],j<-[y] ]

bottomRightDiagonalMoves::Pos->[Pos]
bottomRightDiagonalMoves (x,y) = [ (x+n,y+n) | n<-[1..7], (x+n)<=7, (y+n)<=7 ]

bottomLeftDiagonalMoves::Pos->[Pos]
bottomLeftDiagonalMoves (x,y) = [ (x+n,y-n) | n<-[1..7], (x+n)<=7, (y-n)>=0 ]

topLeftDiagonalMoves::Pos->[Pos]
topLeftDiagonalMoves (x,y) = [ (x-n,y-n) | n<-[1..7], (x-n)>=0, (y-n)>=0 ]

topRightDiagonalMoves::Pos->[Pos]
topRightDiagonalMoves (x,y) = [ (x-n,y+n) | n<-[1..7], (x-n)>=0, (y+n)<=7 ]

-- *************** function to check boundary condition ****************

validateBoundaryCondition::Pos->Bool
validateBoundaryCondition (i,j) = (i<=7 && i>=0 && j<=7 && j>=0)

-- *************** generating semi-valid moves for a given square *****************
-- Positions a piece can move to, given a board and the current position(without caring about check)

genSemiValidMoves::Board->Pos->[Pos]

genSemiValidMoves board pos
		| pieceType piece == Knight = filter (validateSquareForKnight board sqr1) [ x | x<-(genEmptyBoardMoves piece pos) ]
		| pieceType piece == Pawn = filter (findPawnMoves board pos) [ x | x<-(genEmptyBoardMoves piece pos) ]
		| otherwise = filter (clearPath board pos) [ x | x<-(genEmptyBoardMoves piece pos) ]
		where	sqr1 = getPieceOnSquare board pos
			piece = getPiece (sqr1)
			
-- *************** validate pawn moves ***********************

findPawnMoves::Board->Pos->Pos->Bool

findPawnMoves board (x1,y1) (x2,y2)
		| (((x2-x1 == 1)||(x1-x2 == 1)))&&(y1==y2) = isEmpty board (x2,y2)
		| (((x2-x1 == 2)||(x1-x2 == 2)))&&(y1==y2) = (clearPath board (x1,y1) (x2,y2))&&(isEmpty board (x2,y2))
		| otherwise = (isEmpty board (x2,y2) == False)&&(hasOppColor sqr1 sqr2)
		where 	sqr1 = getPieceOnSquare board (x1,y1)
			sqr2 = getPieceOnSquare board (x2,y2)			
			
-- *************** clear path function ***********************

clearPath::Board->Pos->Pos->Bool

clearPath board pos1 pos2
		| (isEmpty board pos2) == True = checkPathInBetween pos1 pos2 board
		| ((isEmpty board pos2) == False)&&(hasOppColor sqr1 sqr2) = checkPathInBetween pos1 pos2 board
		| ((isEmpty board pos2) == False)&&(hasSameColor sqr1 sqr2) = False
		where	sqr1 = getPieceOnSquare board pos1
			sqr2 = getPieceOnSquare board pos2

checkPathInBetween::Pos->Pos->Board->Bool

checkPathInBetween pos1 pos2 board
		| (x1 == x2)&&(y1<y2) = checkHorizontalClearPath (x1,y1+1) pos2 board
		| (x1 == x2)&&(y1>y2) = checkHorizontalClearPath (x1,y1-1) pos2 board
		| (y1 == y2)&&(x1<x2) = checkVerticalClearPath (x1+1,y1) pos2 board
		| (y1 == y2)&&(x1>x2) = checkVerticalClearPath (x1-1,y1) pos2 board
		| (x2 < x1)&&(y2 < y1) = checkDiagonalClearPath (x1-1,y1-1) pos2 board 
		| (x2 < x1)&&(y2 > y1) = checkDiagonalClearPath (x1-1,y1+1) pos2 board
		| (x2 > x1)&&(y2 < y1) = checkDiagonalClearPath (x1+1,y1-1) pos2 board
		| (x2 > x1)&&(y2 > y1) = checkDiagonalClearPath (x1+1,y1+1) pos2 board
		where	x1 = fst pos1
			x2 = fst pos2
			y1 = snd pos1
			y2 = snd pos2
			
checkHorizontalClearPath::Pos->Pos->Board->Bool

checkHorizontalClearPath pos1 pos2 board
		| pos1 == pos2 = True
		| (isEmpty board pos1) == False = False
		| y2 < y1 = checkHorizontalClearPath (x1,y1-1) pos2 board 
		| y2 > y1 = checkHorizontalClearPath (x1,y1+1) pos2 board
		where	x1 = fst pos1
			x2 = fst pos2
			y1 = snd pos1
			y2 = snd pos2
			
checkVerticalClearPath::Pos->Pos->Board->Bool

checkVerticalClearPath pos1 pos2 board
		| pos1 == pos2 = True
		| ((isEmpty board pos1) == False) = False
		| x2 < x1 = checkVerticalClearPath (x1-1,y1) pos2 board 
		| x2 > x1 = checkVerticalClearPath (x1+1,y1) pos2 board
		where	x1 = fst pos1
			x2 = fst pos2
			y1 = snd pos1
			y2 = snd pos2
			
checkDiagonalClearPath::Pos->Pos->Board->Bool

checkDiagonalClearPath pos1 pos2 board
		| pos1 == pos2 = True
		| ((isEmpty board pos1) == False) = False
		| (x2 < x1)&&(y2 < y1) = checkDiagonalClearPath (x1-1,y1-1) pos2 board 
		| (x2 < x1)&&(y2 > y1) = checkDiagonalClearPath (x1-1,y1+1) pos2 board
		| (x2 > x1)&&(y2 < y1) = checkDiagonalClearPath (x1+1,y1-1) pos2 board
		| (x2 > x1)&&(y2 > y1) = checkDiagonalClearPath (x1+1,y1+1) pos2 board
		where	x1 = fst pos1
			x2 = fst pos2
			y1 = snd pos1
			y2 = snd pos2
						
-- *************** generating valid moves for the Board *****************
-- All the valid positions that all the pieces on a Board, can move to

genValidMoves::Board->[[[Pos]]]

genValidMoves board = [map (validatePos board) [ (i,j) | j<-[0..7] ] | i<-[0..7]]

validatePos::Board->Pos->[Pos]

validatePos board pos
	| (isEmpty board pos) = []
	| otherwise = filter (isNotDiscoveredCheck board pos) (genSemiValidMoves board pos)
	
isDiscoveredCheck::Board->Pos->Pos->Bool

isDiscoveredCheck board pos1 pos2
		| null (validateKingCheck newboard color) = False
		| otherwise = True
		where	newboard = movePiece board pos1 pos2
			color = pieceColor (getPiece (getPieceOnSquare board pos1))
			
isNotDiscoveredCheck::Board->Pos->Pos->Bool

isNotDiscoveredCheck board pos1 pos2 = (not)(isDiscoveredCheck board pos1 pos2)

			
-- ******************** functions to check for King's Check *********************

positionHasSameColor::Board->PieceColor->Pos->Bool
positionHasSameColor board c pos = case (getColor (getPieceOnSquare board pos)) of
					Left False -> False
					Right color -> (c == color)

positionHasOppColor::Board->PieceColor->Pos->Bool
positionHasOppColor board c pos = case (getColor (getPieceOnSquare board pos)) of
					Left False -> False
					Right color -> (c /= color)
	
isColoredKing::Board->PieceColor->Pos->Bool
isColoredKing board c pos = ((pieceType (getPiece (getPieceOnSquare board pos))==King))&&(positionHasSameColor board c pos)

isPositionAttackingKing::Board->Pos->Pos->Bool
isPositionAttackingKing board kingPos pos = kingPos `elem` (genSemiValidMoves board pos)

validateKingCheck::Board->PieceColor->[Pos]
validateKingCheck board color =
		let	listOfPiecePos = filter (isNotEmpty board) [ (i,j) | i<-[0..7],j<-[0..7] ]
			oppPiecePos = filter (positionHasOppColor board color) listOfPiecePos
			kingPos = head (filter (isColoredKing board color) listOfPiecePos)
		in filter (isPositionAttackingKing board kingPos) [ pos | pos<-oppPiecePos ]
		
-- ********************** movement functions ***********************

updateList::[a]->Int->(a->a)->[a]
updateList [] _ _ = []
updateList (x:xs) 0 f = (f x):xs
updateList (x:xs) n f = x:updateList xs (n-1) f

updateMatrix::[[a]]->(Int, Int)->a->[[a]]
updateMatrix m (i,j) a = updateList m i (\z->updateList z j (const a))

updateBoard::Board->Pos->PieceOnSquare->Board
updateBoard = updateMatrix

deletePieceOnSquare::Board->Pos->Board
deletePieceOnSquare board p = updateBoard board p emptySquare

-- moves the piece at p1 to p2
movePiece::Board->Pos->Pos->Board
movePiece b p1 p2
	| ((x2==7)||(x2==0))&&(pieceType pc == Pawn) = updateBoard (deletePieceOnSquare b p1) p2 (pawnPromotion color)
	| otherwise = updateBoard (deletePieceOnSquare b p1) p2 (getPieceOnSquare b p1)
	where	x2 = fst p2
		y2 = snd p2
		pc = getPiece (getPieceOnSquare b p1)
		color = pieceColor pc
		
pawnPromotion::PieceColor->PieceOnSquare
pawnPromotion color = Just (Piece Queen color)


genMoves::Board->Pos->[Board]
genMoves board (x1, y1) = map (movePiece board (x1, y1)) ((genValidMoves board)!!x1!!y1)

positionsWithThisColor::PieceColor->Board->[Pos]
positionsWithThisColor color board =
	let	listOfPiecePos = filter (isNotEmpty board) [ (i,j) | i<-[0..7],j<-[0..7] ]
	in filter (positionHasSameColor board color) listOfPiecePos
	
-- *************************** Castling Functions ******************************8
	
isKingSideCastlingPossible::PieceColor->Board->Bool

isKingSideCastlingPossible White board = (checkHorizontalClearPath (7,5) (7,7) board) && (isNotDiscoveredCheck board (7,4) (7,5)) && (isNotDiscoveredCheck board (7,4) (7,6))

isKingSideCastlingPossible Black board = (checkHorizontalClearPath (0,5) (0,7) board) && (isNotDiscoveredCheck board (0,4) (0,5)) && (isNotDiscoveredCheck board (0,4) (0,6))



isQueenSideCastlingPossible::PieceColor->Board->Bool

isQueenSideCastlingPossible White board = (checkHorizontalClearPath (7,3) (7,1) board) && (isNotDiscoveredCheck board (7,4) (7,3)) && (isNotDiscoveredCheck board (7,4) (7,2))

isQueenSideCastlingPossible Black board = (checkHorizontalClearPath (0,3) (0,1) board) && (isNotDiscoveredCheck board (0,4) (0,3)) && (isNotDiscoveredCheck board (0,4) (0,2))



	
whiteCastleKingSide::Board->Board
whiteCastleKingSide board = movePiece (movePiece board (7,4) (7,6)) (7,7) (7,5)

whiteCastleQueenSide::Board->Board
whiteCastleQueenSide board = movePiece (movePiece board (7,4) (7,2)) (7,0) (7,3)

blackCastleKingSide::Board->Board
blackCastleKingSide board = movePiece (movePiece board (0,4) (0,6)) (0,7) (0,5)

blackCastleQueenSide::Board->Board
blackCastleQueenSide board = movePiece (movePiece board (0,4) (0,2)) (0,0) (0,3)



	
castling::Bool->PieceColor->Board->[Board]
castling True _ _ = []
castling False White board
	| w1 && w2 = [whiteCastleKingSide board] ++ [whiteCastleQueenSide board]
	| w1 = [whiteCastleKingSide board]
	| w2 = [whiteCastleQueenSide board]
	| otherwise = []
	where	w1 = isKingSideCastlingPossible White board
		w2 = isQueenSideCastlingPossible White board
castling False Black board
	| b1 && b2 = [blackCastleKingSide board] ++ [blackCastleQueenSide board]
	| b1 = [blackCastleKingSide board]
	| b2 = [blackCastleQueenSide board]
	| otherwise = []
	where	b1 = isKingSideCastlingPossible Black board
		b2 = isQueenSideCastlingPossible Black board
		
hasKingMoved::PieceColor->History->Bool
hasKingMoved c history
	| c == White = kingPositionCheck c (7,4) history
	| otherwise = kingPositionCheck c (0,4) history
	
kingPositionCheck::PieceColor->Pos->History->Bool
kingPositionCheck c kingPos [] = False
kingPositionCheck c kingPos (x:xs)
	| (getPieceOnSquare b kingPos) /= (Just (Piece King c)) = True
	| otherwise = kingPositionCheck c kingPos xs
	where	c = fst x
		b = snd x

-- ********************** En-Passant functions ***********************

isEnPassantPossible::BoardState->BoardState->[Pos]
isEnPassantPossible (c2,b2) (c1,b1)
		| (c1 == White) = filter (wasTwoStepJump c1 b2 b1) whitePawnsInEnPassantPos
		| (c1 == Black) = filter (wasTwoStepJump c1 b2 b1) blackPawnsInEnPassantPos
		where	listOfWhiteEnPassantPos = filter (isNotEmpty b2) [ (i,j) | i<-[4],j<-[0..7] ]
			whitePawnsInEnPassantPos = filter (isColoredPawn b2 c1) listOfWhiteEnPassantPos
			listOfBlackEnPassantPos = filter (isNotEmpty b2) [ (i,j) | i<-[3],j<-[0..7] ]
			blackPawnsInEnPassantPos = filter (isColoredPawn b2 c1) listOfBlackEnPassantPos
			
wasTwoStepJump::PieceColor->Board->Board->Pos->Bool
wasTwoStepJump White b2 b1 (x,y) = (isEmpty b2 (x+2,y))&&(isColoredPawn b1 White (x+2,y))
wasTwoStepJump Black b2 b1 (x,y) = (isEmpty b2 (x-2,y))&&(isColoredPawn b1 Black (x-2,y))
			
isColoredPawn::Board->PieceColor->Pos->Bool
isColoredPawn board color pos
		| isNotEmpty board pos = (pieceType (getPiece (getPieceOnSquare board pos))==Pawn)&&(positionHasSameColor board color pos)
		| otherwise = False

enPassant::BoardState->Pos->[Board]
enPassant (c, b) (x,y)
	| c == White = [movePiece (deletePieceOnSquare b (x,y)) pos1 (x-1,y) | pos1<-requiredPawns]
	| c == Black = [movePiece (deletePieceOnSquare b (x,y)) pos1 (x+1,y) | pos1<-requiredPawns]
	where	requiredPawns = filter (isColoredPawn b c) [(i,j) | i<-[x],j<-[y-1,y+1],j>=0,j<=7]
	
-- *********************************************************************

-- ********************* functions to generate the next possible states in the game *****************************

nextStatesBasic::BoardState->[BoardState]
nextStatesBasic (c, b) = [(oppositeColor c, b')|pos<-positionsWithThisColor c b, b'<-genMoves b pos]


-- ********* Generates the basic next states plus castling states and en-passant states ***********
nextStatesAdvanced::GameState->[BoardState]

nextStatesAdvanced (currState, []) = nextStatesBasic currState

nextStatesAdvanced (currState, history)
		| null enPassantDetails = nextStatesBasic currState ++ [(oppositeColor whoseTurn, b')|b'<-castling kingMoved whoseTurn b]
		
		| otherwise = nextStatesBasic currState ++ [(oppositeColor whoseTurn, b')|b'<-castling kingMoved whoseTurn b] ++ [(oppositeColor whoseTurn, b')|b'<-enPassant currState (head enPassantDetails)]
		
		where	whoseTurn = fst currState
			b = snd currState
			kingMoved = hasKingMoved whoseTurn history
			enPassantDetails = isEnPassantPossible currState (last history)

-- **************************************************************************************************************

-- ************************** functions to play the game *******************************
--gen = getStdGen

getRandomNextState::GameState->Int->BoardState
getRandomNextState gs t =
		let	gen = mkStdGen t
			states = nextStatesAdvanced gs
			size = length states
			(index, newgen) = randomR (0, size-1) gen :: (Int, StdGen)
		in (states!!index)


-- *************************************************************************************

-- initial boards

initialBoard, emptyBoard, exampleBoard1::Board
initialBoard = [[Just (Piece Rook Black), Just (Piece Knight Black), Just (Piece Bishop Black), Just (Piece Queen Black), Just (Piece King Black), Just (Piece Bishop Black), Just (Piece Knight Black), Just (Piece Rook Black)],
                [Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black)],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White)],
                [Just (Piece Rook White), Just (Piece Knight White), Just (Piece Bishop White), Just (Piece Queen White), Just (Piece King White), Just (Piece Bishop White), Just (Piece Knight White), Just (Piece Rook White)]]
                
exampleBoard1 = [[Just (Piece Rook Black), Nothing, Just (Piece Bishop Black), Nothing, Just (Piece King Black), Just (Piece Bishop Black), Just (Piece Knight Black), Just (Piece Rook Black)],
                [Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Nothing, Just (Piece Pawn Black), Just (Piece Pawn Black), Nothing, Just (Piece Pawn Black)],
                [Nothing, Nothing, Nothing, Just (Piece Pawn Black), Nothing, Nothing, Just (Piece Pawn Black), Nothing],
                [Just (Piece Queen Black), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Just (Piece Pawn White), Just (Piece Knight Black), Nothing, Nothing, Nothing],
                [Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Queen White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White)],
                [Just (Piece Rook White), Just (Piece Knight White), Just (Piece Bishop White), Nothing, Just (Piece King White), Just (Piece Bishop White), Just (Piece Knight White), Just (Piece Rook White)]]

exampleState1::BoardState              
exampleState1 = (White, exampleBoard1)

initialState::BoardState
initialState = (White, initialBoard)

initialGameState::GameState
initialGameState = (initialState, [])

emptyBoard = [[Nothing|_<-[1..8]]|_<-[1..8]]
