module Main (main) where

import Text.Megaparsec
import Hurtle.Parser
-- This is the main entry point for your Hurtle viewer.


main :: IO ()
main = do
    text <- readFile "examples/passing/03-worm.hogo"
    case parse parseHogo "" text of
        (Left err) -> putStrLn $ errorBundlePretty err
        (Right x) -> print x



--main = error "Not implemented!\n(find me in app/Main.hs)\n"