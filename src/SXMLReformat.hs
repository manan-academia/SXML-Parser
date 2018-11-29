{-

Name: Manan Joshi
Time spent on assignment:
Collaborators/Acknowledgements:

-}

{-# OPTIONS -Wall -Wno-unused-imports -Wno-unused-do-bind #-}

module SXMLReformat where

{-
Useful Prelude types and functions.

getContents :: IO String
-}

import           Data.Char

import           System.Environment (getArgs)
{-
Useful Stytem.Environment functions.

getArgs :: IO [String]
-- Computation `getArgs` returns a list of the program's command line arguments (not including the program name).
Note: When executing a Haskell program as `runhaskell Prog.hs arg1 arg2 arg3`, `getArgs` will return `[arg1, arg2, arg3]`.
-}

import           Text.Read          (readMaybe)
{-
Useful Text.Read functions.

readMaybe :: Read a => String -> Maybe a
-- Parse a string using the `Read` instance. Succeeds if there is exactly one valid result.
-}


import           Parser
import           PrettyPrint


newtype SXML = SXML Elt deriving (Eq, Read, Show)

data Elt = Elt String [Att] [Item] deriving (Eq, Read, Show)
-- Invariant:: forall (Elt n atts items) . validName n

data Att = Att String String deriving (Eq, Read, Show)
-- Invariant:: forall (Att n v) . validName n && validAttValue v

data Item = IElt Elt | IText String deriving (Eq, Read, Show)
-- Invariant:: forall (IText s) . validText s

validName :: String -> Bool
validName []     = False
validName (c:cs) = nameStartChar c && all nameChar cs
    where nameStartChar = (== ':') ||| (== '_') ||| isAlpha
          nameChar = nameStartChar ||| (== '-') ||| (== '.') ||| isDigit

validAttValue :: String -> Bool
validAttValue = all ((/= '<') &&& (/= '>') &&& (/= '"') &&& (not . isSpace ||| (== ' ')))

validText :: String -> Bool
validText = (not . null) &&& all ((/= '<') &&& (/= '>') &&& (not . isSpace))

infixr 2 |||
(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f ||| g = \ x -> f x || g x
infixr 3 &&&
(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f &&& g = \ x -> f x && g x


parseComment :: Parser ()
parseComment = between (string "<!--") (string "-->") (skipManyA (string " "))


parseMisc :: Parser ()
parseMisc = skipManyA $ choice [skipSpaces, parseComment]


parseAttribute :: Parser Att
parseAttribute = do
                  name <- parseName
                  char '=' >> char '\"'
                  value <- parseValue
                  skipSpaces
                  char '\"'
                  return (Att name value)

parseName :: Parser String
parseName = undefined

parseValue :: Parser String
parseValue = undefined

parseItem :: Parser Item
parseItem = choice [elt, text']
    where
      elt   = IElt  <$> parseElement
      text' = IText <$> parseText


parseText :: Parser String
parseText = undefined


parseEmptyTagElt :: Parser Elt
parseEmptyTagElt = do
                    char '<'
                    s <- manyL letter
                    skipSpaces
                    a <- manyL parseAttribute
                    skipSpaces
                    i <- manyL parseItem
                    skipSpaces >> token (string "/>")
                    return (Elt s a i)

parseStartTag :: Parser (String, [Att])
parseStartTag = do
                  skipSpaces >> char '<'
                  tag <- manyL letter
                  skipSpaces
                  atts <- manyL parseAttribute
                  token $ char '>'
                  return (tag, atts)

parseEndTag :: String -> Parser ()
parseEndTag s = do
                 string "</"
                 skipSpaces
                 string s
                 skipSpaces
                 char '>'
                 return ()


parseSETagElt :: Parser Elt
parseSETagElt = do
                  (tag, attrs) <- parseStartTag
                  items <- manyL parseItem
                  parseEndTag tag
                  return (Elt tag attrs items)


parseElement :: Parser Elt
parseElement = choice [parseEmptyTagElt, parseSETagElt]


sxmlP :: Parser SXML
sxmlP = SXML <$> do
                   parseMisc
                   e <- parseElement
                   parseMisc
                   return e


sxmlD :: SXML -> Doc
sxmlD = undefined

{-

Describe your `sxmlD`.  That is, give a description of the pretty-printed SXML
documents produced by `sxmlD`.  Comment on any design decisions that you made in
developing `sxmlD`.  Comment on any "ugly" aspects that remain in pretty-printed
SXML documents produced by `sxmlD`.

-----

<<Your answer here.>>

-}



main :: IO ()
main = undefined

{-

$ runhaskell SXMLReformat < gettysburg.sxml

$ runhaskell sXMLReformat 40 < gettysburg.sxml

-}
