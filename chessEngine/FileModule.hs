module FileModule where

import System.IO
import qualified Data.ByteString as Str
import qualified Data.ByteString.Char8 as Char8

loadFromFile::FilePath -> IO String
loadFromFile fname = do {
        contents <- Str.readFile fname;
        return (Char8.unpack contents);
    }

appendToFile::FilePath -> String -> IO()
appendToFile fname str = do {
        Str.appendFile fname (Char8.pack(str++"\n"))
    }

writeToFile::FilePath -> String -> IO()
writeToFile fname str = do {
        Str.writeFile fname (Char8.pack(str++"\n"))
    }
