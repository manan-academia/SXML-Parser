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

import           Control.Monad
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


parseText :: Parser String
parseText = skipSpaces >> many1L aux
    where
      aux = satisfy (\ c -> not (c == '<' || c == '>' || c == ' '))


parseItem :: Parser Item
parseItem = choice [elt, text']
    where
      elt   = IElt  <$> (skipSpaces >> parseElement)
      text' = IText <$> parseText


parseAttValue :: Parser String
parseAttValue = skipSpaces >> manyL aux
    where
      aux = satisfy (\ c -> not (c == '<' || c == '>' || c == '\"'))


parseAttribute :: Parser Att
parseAttribute = do
                  name <- char ' ' >> skipSpaces >> many1L letter
                  char '=' >> char '\"'
                  value <- skipSpaces >> parseAttValue
                  skipSpaces >> char '\"'
                  return (Att name value)


parseName :: Parser String
parseName = many1L (choice [char ':', char '_', letter])
            +++
            manyL (choice [char ':', char '_', char '-', char '.', alphanum])


parseEndTag :: String -> Parser ()
parseEndTag s = do
                 skipSpaces
                 string "</"
                 string s
                 skipSpaces
                 token $ char '>'
                 return ()


parseStartTag :: Parser (String, [Att])
parseStartTag = do
                  skipSpaces >> char '<'
                  tag <- manyL letter
                  atts <- manyL parseAttribute
                  skipSpaces
                  token $ char '>'
                  return (tag, atts)


parseSETagElt :: Parser Elt
parseSETagElt = do
                  (tag, attrs) <- parseStartTag
                  -- parseMisc
                  items <- {- parseMisc >> -} skipSpaces >> manyL parseItem
                  -- parseMisc
                  skipSpaces
                  parseEndTag tag
                  return (Elt tag attrs items)


parseEmptyTagElt :: Parser Elt
parseEmptyTagElt = do
                    char '<'
                    s <- manyL letter
                    --skipSpaces
                    a <- manyL parseAttribute
                    skipSpaces >> token (string "/>")
                    return (Elt s a [])


parseElement :: Parser Elt
parseElement = choice [parseEmptyTagElt, parseSETagElt]

parseComment :: Parser ()
parseComment = between (string "<!--") (string "-->") (skipManyA (manyL aux))
    where
      aux = satisfy (\ c -> not (c == '-' && c == '-'))


parseMisc :: Parser ()
parseMisc = skipManyA $ choice [skipMany1A skipSpaces, parseComment]


sxmlP :: Parser SXML
sxmlP = SXML <$> do
                   --parseMisc
                   skipSpaces
                   e <- parseElement
                   skipSpaces
                   --parseMisc
                   return e


printItems :: [Item] -> Doc
printItems []             = empty
printItems (IText x : xs) = text " " <+> text x <+> text " " <+> printItems xs
printItems (IElt x : xs)  = printElement x <+> printItems xs


printAttributes :: [Att] -> Doc
printAttributes xs = foldr (\ x acc -> text " " <+> (text (fst $ decompose x) <+> text "=\""
                                                <+> text (snd $ decompose x) <+> text "\"") <+> acc) empty xs

    where
      decompose (Att name value) = (name, value)


printElement :: Elt -> Doc
printElement (Elt name attrs items) = group $ text "<" <+> text name <+> printAttributes attrs <+>
                                                 if null items
                                                   then text " />"
                                                   else text ">" <+> slbreak <+> nest 2 (printItems items)
                                                    <+> text "<" <+> text "/"
                                                    <+> text name <+> text ">"


sxmlD :: SXML -> Doc
sxmlD (SXML ast) = printElement ast

{-

Describe your `sxmlD`.  That is, give a description of the pretty-printed SXML
documents produced by `sxmlD`.  Comment on any design decisions that you made in
developing `sxmlD`.  Comment on any "ugly" aspects that remain in pretty-printed
SXML documents produced by `sxmlD`.

-----

<<Your answer here.>>

-}



main :: IO ()
main = do
        content <- getContents
        --print $ parseMaybe sxmlP (join (lines content))
        case parseMaybe sxmlP (join $ lines content) of
           Just x  -> pprint 60 (sxmlD x)
           Nothing -> putStrLn "** PARSE ERROR **"
        return ()

{-

$ runhaskell SXMLReformat < gettysburg.sxml

$ runhaskell sXMLReformat 40 < gettysburg.sxml

-}

sampleText :: String
sampleText = "<document>" ++
  "<section number=\"1\">" ++
    "<p color=\"red\">" ++
      "This paragraph" ++
      "is <b>red</b> " ++
      "." ++
    "</p>"++
    "<p size=\"20\">"++
      "This paragraph" ++
      "is <b>big</b>" ++
      "."++
    "</p>"++
    "<p font=\"Times\""++
       " color=\"red\""++
       " size=\"20\"" ++
       " align=\"center\">"++
      "This paragraph" ++
      "is" ++
      "<b>" ++
        "quite fancy"++
      "</b>" ++
      "." ++
    "</p>"++
    "<img src=\"logo.png\"" ++
         " alt=\"logo\"/>" ++
  "</section>" ++
  "</document>"
