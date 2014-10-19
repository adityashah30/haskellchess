import UCI
import MoveModule
import Board
import Search
--import OpeningBook
--import OpenBookParser

main :: IO ()
main = uci
--main = do{
--        displayBoard(snd(fst(playGameUsingHistory initialGameState moves)))
--    } 
--    where moves = ["d2d4", "d7d5","c2c4", "e7e6","b1c3", "g8f6","c1g5", "f8e7"]
--main = do{
--        print (searchBook moves)    
--    }
--    where   moves = ["d2d4", "d7d5","c2c4", "e7e6","b1c3", "g8f6","c1g5", "f8e7"]
--main = do{
--        displayGame gs
--    } 
--    where 
--        moves = ["d2d4", "d7d5", "c2c4", "d5c4", "g1f3", "g8f6", "e2e3", "c8g4", "f1c4", "e7e6", "d1a4", "c7c6", "h1f1", "b7b5", "c4b5", "c6b5", "a4b5", "d8d7", "b5d7", "b8d7", "f1h1", "f8b4", "c1d2", "b4d2", "b1d2", "e8e7", "h2h3", "g4f3", "g2f3", "h8c8", "h1h2", "c8c2", "a1b1", "a8b8", "e1d1", "b8b2", "b1b2", "c2b2", "a2a4", "b2a2", "h2g2", "g7g6", "a4a5", "a2a5", "d1c1", "f6d5", "c1b2", "d7b6", "g2g5", "h7h6", "g5g1", "g6g5", "g1g4", "f7f5", "g4g1", "b6a4", "b2a1", "a4c3", "a1b2", "a5a2", "b2b3", "a2d2", "g1a1", "d2f2", "a1a7", "e7f6", "f3f4", "g5f4", "h3h4", "f4e3", "a7a8", "e3e2", "a8f8", "f6e7", "f8h8", "e2e1q", "h8h7", "e7d6"]
--        gs = playGameUsingHistory initialGameState moves
