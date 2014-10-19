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
  gameState <- if posType == "fen" then return initialGameState else return initialGameState
  spaces
  liftA CmdPosition $ option gameState (string "moves" >> parserMoveList gameState)
  where
    parserMoveList gameState = do
      mm <- optionMaybe (spaces >> parserMove)
      case mm of
        Just (from, to)  -> parserMoveList $ makeMove gameState (from, to)
        Nothing -> return gameState

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
