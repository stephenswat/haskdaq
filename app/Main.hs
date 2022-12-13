module Main (main) where

import System.Environment (getArgs)
import Data.ByteString (readFile)
import Text.Show.Pretty (ppShow)
import Data.Attoparsec.ByteString (parseOnly)
import Data.Haskdaq.EventStorage (parserFileContents)
import Data.Haskdaq.EFormat (parserDataRecord)

main :: IO ()
main = do
    fname <- getArgs;
    fread <- Data.ByteString.readFile (fname !! 0);
    case parseOnly (parserFileContents parserDataRecord) fread of
        (Left e) -> error ("Parse failed: " ++ e)
        (Right v) -> putStr ((ppShow v) ++ "\n")
