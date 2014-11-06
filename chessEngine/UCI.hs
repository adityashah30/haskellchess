module UCI
       ( uci
       ) where

import           Control.Applicative (liftA)
import           Data.List
import		 Data.Char
import           Data.IORef
import           System.Exit
import           Data.Maybe
import           System.IO

import           Text.ParserCombinators.Parsec
import           Evaluator
import           Board
import           Search
import		       Pieces
import           FileModule
import           MoveModule


data SearchOption = MovetimeMsc Int | Infinity deriving (Show)
                    
data Command = CmdUci 
             | CmdIsReady 
             | CmdUciNewGame 
             | CmdPosition GameState
             | CmdGo SearchOption
             | CmdStop
             | CmdQuit

data Response = RspId String String
              | RspUciOk
              | RspReadyOk
              | RspBestMove String
              | RspInfo String
              | RspOption String 
            

---------------- show ------------------
instance Show Response where
  show RspUciOk           = "uciok"
  show RspReadyOk         = "readyok"
  show (RspInfo info)     = "info " ++ info
  show (RspId name value) = "id " ++ name ++ " " ++ value
 -- show (RspBestMove move) = "bestmove " ++ "q"
  show (RspBestMove move) = "bestmove " ++ move
  show (RspOption text)   = "option " ++ text


------------------ parsers --------------
uciUciParser :: Parser Command
uciUciParser = string "uci" >> return CmdUci

uciIsReadyParser :: Parser Command
uciIsReadyParser = string "isready" >> return CmdIsReady

uciNewGameParser :: Parser Command
uciNewGameParser = string "ucinewgame" >> return CmdUciNewGame

uciStopParser :: Parser Command
uciStopParser = string "stop" >> return CmdStop

uciQuitParser :: Parser Command
uciQuitParser = string "quit" >> return CmdQuit

uciIntParser :: Parser Int
uciIntParser = liftA read $ many1 digit

uciGoParser :: Parser Command
uciGoParser = do
                string "go" >> spaces
                mbTimeout <- optionMaybe (string "movetime" >> spaces >> uciIntParser)
                return $ case mbTimeout of
                            Nothing -> CmdGo Infinity
                            Just timeout -> CmdGo $ MovetimeMsc timeout
                    
    
uciPositionParser :: CharParser () Command
uciPositionParser = do
  _ <- string "position" >> (many1 $ char ' ')
  posType <- string "fen" <|> string "startpos"
  spaces
  gameState <- return initialGameState
  spaces
  liftA CmdPosition (option gameState (gs posType gameState))
  where
    gs posType gameState = case posType of
        "fen" ->  (spaces >> fenParser gameState)
        "startpos" -> (string "moves" >> parserMoveList gameState)
    parserMoveList gameState = do
      mm <- optionMaybe (spaces >> parserMove)
      case mm of
        Just (from, to)  -> parserMoveList $ makeMove gameState (from, to)
        Nothing -> return gameState
    fenParser gameState = do
      bs <- fenBoardStateParser
      return (bs, [(White, emptyBoard)])

fenBoardStateParser :: Parser BoardState
fenBoardStateParser = do
  board <- fenBoardParser
  spaces
  c <- string "w" <|> string "b"
  spaces
  k1 <- string "K" <|> string ""
  q1 <- string "Q" <|> string ""
  k2 <- string "k" <|> string ""
  q2 <- string "q" <|> string ""
  spaces
  ok <- optionMaybe parserSquare
  hy <- optionMaybe (string "-")
  spaces
  halfmove <- oneOf ['0' .. ]
  spaces
  fullmove <- oneOf ['0' .. ]
  return (bs c board)
  where 
    bs c board = ((getColor c), board)
    getColor c = case c of 
      "w" -> White
      "b" -> Black


fenBoardParser :: Parser Board
fenBoardParser = do
  rank8 <- fenRowParser
  rank7 <- fenRowParser
  rank6 <- fenRowParser
  rank5 <- fenRowParser
  rank4 <- fenRowParser
  rank3 <- fenRowParser
  rank2 <- fenRowParser
  rank1 <- fenRowParser
  return ([rank8]++[rank7]++[rank6]++[rank5]++[rank4]++[rank3]++[rank2]++[rank1])

fenRowParser :: Parser [PieceOnSquare]
fenRowParser = do
  mm <- optionMaybe fenPieceParser
  case mm of
    Just x -> do 
      arr <- fenRowParser
      return (x ++ arr)
    Nothing -> do
      _ <- string "/" <|> string " "
      return []


fenPieceParser :: Parser [PieceOnSquare]
fenPieceParser = do
  character <- oneOf ['r', 'n', 'b', 'q', 'k', 'p', 'R', 'N', 'B', 'Q', 'K', 'P', '1', '2','3','4','5','6','7','8']
  case character of
    'r' -> return [Just (Piece Rook Black)]
    'n' -> return [Just (Piece Knight Black)]
    'b' -> return [Just (Piece Bishop Black)]
    'q' -> return [Just (Piece Queen Black)]
    'k' -> return [Just (Piece King Black)]
    'p' -> return [Just (Piece Pawn Black)]
    'R' -> return [Just (Piece Rook White)]
    'N' -> return [Just (Piece Knight White)]
    'B' -> return [Just (Piece Bishop White)]
    'Q' -> return [Just (Piece Queen White)]
    'K' -> return [Just (Piece King White)]
    'P' -> return [Just (Piece Pawn White)]
    '1' -> return [Nothing]
    '2' -> return [Nothing, Nothing]
    '3' -> return [Nothing, Nothing, Nothing]
    '4' -> return [Nothing, Nothing, Nothing, Nothing]
    '5' -> return [Nothing, Nothing, Nothing, Nothing, Nothing]
    '6' -> return [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    '7' -> return [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    '8' -> return [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]


-- | On a given board parses an UCI protocol style move notation into Move
parserMove :: Parser (Pos,Pos)
parserMove = do
  from <- parserSquare
  to <- parserSquare
  queen <- parserQueen
  return (from, to)

-- | The parser for 'a1' etc notation
parserSquare :: Parser Pos
parserSquare = do
  x <- oneOf ['a' .. 'h']
  y <- oneOf ['1' .. '8']
  return (7 - (ord y - ord '1'), ord x - ord 'a')

parserQueen :: Parser Char
parserQueen = do
  q <- string "q" <|> string ""
  return 'q'

uciCmdParser :: Parser Command
uciCmdParser = try uciNewGameParser
               <|> uciUciParser
               <|> uciIsReadyParser
               <|> uciStopParser
               <|> uciQuitParser
               <|> uciGoParser
               <|> uciPositionParser


parseCommand :: String -> Maybe Command
parseCommand line = case parse uciCmdParser "" line of
                Left _ -> Nothing
                Right cmd -> Just cmd
                

logFilePath::FilePath
logFilePath = "../data/log.txt"

-- | The main IO () UCI loop. Talks to an UCI interface and drives the engine
uci :: IO ()
uci = do
    writeToFile logFilePath ""
    hSetBuffering stdout NoBuffering
    lastGameState <- newIORef initialGameState

    let dialogue = do
                line <- getLine
                appendToFile logFilePath "XBoard:"
                appendToFile logFilePath line
                case parseCommand line of
                    Nothing -> return ()
                    Just cmd -> do 
                                    responses <- getResponse cmd
                                    let output = intercalate "\n" $ map show responses
                                    appendToFile logFilePath "Engine:"
                                    appendToFile logFilePath output
                                    putStrLn output
                dialogue
                where
                    getResponse CmdUci = return [RspId "name" "Chess", RspId "author" "Aditya-Shivaram-Nitin-Anish", RspUciOk]
                    getResponse CmdIsReady = return [RspReadyOk]
                    getResponse CmdUciNewGame = return []
                    getResponse CmdQuit = exitSuccess
                    getResponse CmdStop = return []
                    getResponse (CmdPosition gs) = do
                      modifyIORef lastGameState (const gs)
                      --displayGame gs
                      return []
                    getResponse (CmdGo _) = do
                      g <- readIORef lastGameState
                      -- (pv, p') <- runSearch (search 4) p
                      -- writeIORef lastPosition p'
                      let	bs = getNextState g
                      let m = genMovesString (fst g) bs
                      return [ RspBestMove m ]
    dialogue
