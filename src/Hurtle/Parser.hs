module Hurtle.Parser where

import Hurtle.Types

-- You'll probably want to refer to https://hackage.haskell.org/package/megaparsec for documentation of the Megaparsec library.
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char (isDigit)
import Control.Monad (void)
import Text.Megaparsec.Char.Lexer (skipLineComment)

parseHogo :: Parser [HogoCode]
parseHogo = filter (/= Comment) <$> (parseLine `sepBy` newline <* optional newline <* eof)

parseLine :: Parser HogoCode
parseLine = try parseForward 
        <|> try parseBackward
        <|> try parseRight
        <|> try parseLeft
        <|> try parsePenup
        <|> try parsePendown
        <|> try parseHome
        <|> try parseClearscreen
        <|> try parseRepeat
        <|> parseComment


parseForward :: Parser HogoCode
parseForward = do 
    space
    _ <- try $ char 'F' <|> char 'f'
    _ <- string "orward"
    hspace
    GoForward <$> parseFloat

parseBackward :: Parser HogoCode
parseBackward = do
    space
    _ <- try $ char 'B' <|> char 'b'
    _ <- string "ack"
    hspace
    GoBackward <$> parseFloat

parseRight :: Parser HogoCode
parseRight = do
    space
    _ <- try $ char 'R' <|> char 'r'
    _ <- string "ight"
    hspace
    TurnRight <$> parseFloat

parseLeft :: Parser HogoCode
parseLeft = do
    space
    _ <- try $ char 'L' <|> char 'l'
    _ <- string "eft"
    hspace
    TurnLeft <$> parseFloat

parseComment :: Parser HogoCode
parseComment = do
    space 
    skipLineComment ";"
    pure Comment

-- parseEmptyLine :: Parser ()
-- parseEmptyLine = do
--     space
--     _ <- newline
--     pure ()

-- parseCandE :: Parser ()
-- parseCandE = skipMany (parseComment <|> parseEmptyLine) 

parsePenup :: Parser HogoCode
parsePenup = do 
    space
    _ <- try $ char 'P' <|> char 'p'
    _ <- string "en"
    _ <- try $ char 'U' <|> char 'u' 
    _ <- char 'p'
    pure PenUp   

parsePendown :: Parser HogoCode
parsePendown = do 
    space
    _ <- try $ char 'P' <|> char 'p'
    _ <- string "en"
    _ <- try $ char 'D' <|> char 'd' 
    _ <- string "own"
    pure PenDown

parseHome :: Parser HogoCode
parseHome = do
    space
    _ <-  try $ char 'H' <|> char 'h'
    _ <- string "ome"
    pure GoHome

parseClearscreen :: Parser HogoCode
parseClearscreen = do
    space
    _ <- try $ char 'C' <|> char 'c'
    _ <- string "lear"
    _ <- try $ char 'S' <|> char 's'
    _ <- string "creen"
    pure ClearScreen

parseRepeat :: Parser HogoCode
parseRepeat = do
    space
    _ <- try $ char 'R' <|> char 'r'
    _ <- string "epeat"
    hspace
    x <- parseInt
    space
    _ <- char '['
    space
    y <- parseHogo
    space
    _ <- char ']'
    pure (Repeat x y)

{- 
We want the option to parse an int or a float so we start with a parser for numbers
but it keeps it as strings  
-}
parseNumberAsString :: Parser String
parseNumberAsString = do takeWhileP Nothing isDigit

{- 
Using fmap we can change a Parser String into a Parser Int
-}
parseInt :: Parser Int
parseInt = read <$> parseNumberAsString

{- 
The parser will have floats represented as 1 or 1.0. It does so by creating a Parser String
of the float using <$> and <*> to connect the Parser Strings. In order to implement the 
leniency we use the option parser that looks for one of two options, the end of the number
or a '.'. If it finds a '.' then it connects it to the numbers after it and add all of that 
to the Parser String 
-}
parseFloat :: Parser Float
parseFloat = read <$> floatAsParserString
    where
        floatAsParserString = (++) <$> parseNumberAsString <*> option "" ((:) <$> char '.' <*> parseNumberAsString)




    
    