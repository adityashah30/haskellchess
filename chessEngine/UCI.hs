module UCI
       ( uci
       ) where

import Data.List
import Data.List.Split
import Data.IORef
import System.Exit
import Data.Maybe
import System.IO
import ParserModule
import Evaluator
import Board
import Search
import Search2
import Pieces
import FileModule
import MoveModule
import OpenBookModule
import LearningModule

data SearchOption = MovetimeMsc Int | Infinity deriving (Show)
                    
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

logFilePath::FilePath
logFilePath = "../data/log.txt"

lastGamePath::FilePath
lastGamePath = "../data/lastgame.txt"

-- | The main IO () UCI loop. Talks to an UCI interface and drives the engine
uci :: OpeningBook -> IO ()
uci openingBook = do
    writeToFile logFilePath ""
    lastMove <- loadFromFile lastGamePath
    modifyKB lastMove
    hSetBuffering stdout NoBuffering
    lastGameState <- newIORef initialGameState

    let dialogue = do
                line <- getLine
                appendToFile logFilePath "XBoard:"
                appendToFile logFilePath line
                writeToFile lastGamePath line
                case parseCommand line of
                    Nothing -> return ()
                    Just cmd -> do 
                                  responses <- getResponse cmd
                                  let output = intercalate "\n" $ map show responses
                                  appendToFile logFilePath "Engine:"
                                  appendToFile logFilePath output
                                  appendToFile lastGamePath output
                                  putStrLn output
                case parseCommand line of
                    Nothing -> return ()
                    Just (CmdPosition gs) -> do
                                          writeToFile lastGamePath line
                    _ -> return ()
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
                      let	bs = getNextState g openingBook
                      let m = genMovesString (fst g) bs
                      return [ RspBestMove m ]
    dialogue
