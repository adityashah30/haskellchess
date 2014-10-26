module FileModule where

import System.IO
import qualified Data.ByteString as Str
import qualified Data.ByteString.Char8 as Char8

--Given a filepath, load the contents in String format.
loadFromFile::FilePath -> IO String
loadFromFile fname = do {
        contents <- Str.readFile fname;
        return (Char8.unpack contents);
    }

--Given a filepath, append to a file in String format.
appendToFile::FilePath -> String -> IO()
appendToFile fname str = do {
        Str.appendFile fname (Char8.pack(str++"\n"))
    }

--Given a filepath, write to a file in String format. It overwrites an existing file.
writeToFile::FilePath -> String -> IO()
writeToFile fname str = do {
        Str.writeFile fname (Char8.pack(str++"\n"))
    }
