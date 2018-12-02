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
parseText = many1L aux
    where
      aux = satisfy (\ c -> not (c == '<' || c == '>' || isSpace c))


parseItem :: Parser Item
parseItem = choice [elt, text']
    where
      elt   = IElt  <$> (skipSpaces >> parseElement)
      text' = IText <$> parseText


parseAttValue :: Parser String
parseAttValue = manyL (char ' ' +++ aux)
    where
      aux = satisfy (\ c -> not (c == '<' || c == '>' || c == '\"' || isSpace c))


parseAttribute :: Parser Att
parseAttribute = do
                  name <- parseName
                  char '=' >> char '\"'
                  value <- parseAttValue
                  char '\"'
                  return (Att name value)


parseName :: Parser String
parseName = do
             f <- choice [char ':', char '_', letter]
             r <- manyL aux
             return (f : r)
    where
      aux = choice [char ':', char '-', char '_', char '.', alphanum]


parseEndTag :: String -> Parser ()
parseEndTag s = do
                 string "</"
                 string s
                 skipSpaces
                 char '>'
                 return ()


parseStartTag :: Parser (String, [Att])
parseStartTag = do
                  char '<'
                  tag <- parseName
                  atts <- manyL (skipMany1L (char ' ') >> parseAttribute)
                  skipSpaces >> char '>'
                  return (tag, atts)


parseSETagElt :: Parser Elt
parseSETagElt = do
                  (tag, attrs) <- parseStartTag
                  items <- manyL (manyL parseMisc >> parseItem)
                  manyL parseMisc
                  parseEndTag tag
                  return (Elt tag attrs items)


parseEmptyTagElt :: Parser Elt
parseEmptyTagElt = do
                    char '<'
                    s <- parseName
                    a <- manyL (skipMany1L (char ' ') >> parseAttribute)
                    skipSpaces >> string "/>"
                    return (Elt s a [])


parseElement :: Parser Elt
parseElement = choice [parseEmptyTagElt, parseSETagElt]


parseComment :: Parser ()
parseComment = between (string "<!--") (string "-->") (skipManyL aux)
    where
      aux = condition +++ (char '-' >> condition)
      condition = satisfy (/= '-')


parseSpaces :: Parser ()
parseSpaces = skipMany1L space


parseMisc :: Parser ()
parseMisc = parseComment +++ parseSpaces


sxmlP :: Parser SXML
sxmlP = SXML <$> do
                   manyL parseMisc
                   e <- parseElement
                   manyL parseMisc
                   return e


printItems :: [Item] -> Doc
printItems []             = empty
printItems xs = fillSep (printItems' <$> xs)
    where
      printItems' (IText x) = text x
      printItems' (IElt x)  = printElement x True


printAttributes :: [Att] -> Doc
printAttributes [] = empty
printAttributes xs = text " " <+> fillSep ((<$>) decompose xs)
    where
      decompose (Att name value) = text name <+> text "=" <+> text "\""
                               <+> text value <+> text "\""



printElement :: Elt -> Bool -> Doc
printElement (Elt name attrs items) c = group $ do
                                          let start = if c
                                                        then nest (length name + 2) startTag
                                                        else startTag
                                          let d2 = if null items then empty else lbreak <+>
                                                                  nest (length name + 2) (printItems items)
                                                              <+> slbreak
                                          let end = if null items then empty else endTag
                                          vsep [start, d2, end]
    where
      startTag = text "<" <+> text name
             <+> nest (length name + 2) (printAttributes attrs)
             <+> if null items
                  then text " />" <+> slbreak
                  else text ">" <+> slbreak
      endTag = slbreak <+> text "<" <+> text "/"
           <+> text name <+> text ">" <+> lbreak


sxmlD :: SXML -> Doc
sxmlD (SXML ast) = printElement ast False

{-

Describe your `sxmlD`.  That is, give a description of the pretty-printed SXML
documents produced by `sxmlD`.  Comment on any design decisions that you made in
developing `sxmlD`.  Comment on any "ugly" aspects that remain in pretty-printed
SXML documents produced by `sxmlD`.

-----

sxmlD starts of with printing the initial element. On encountering an attribute/s it hands of
the control to the function that generates attribute doc (printAttributes). printAttributes tries
its best to fill the content to the given width and when the content is exceeded it jumps of to
the next line with indentation. After the attributes are printed depending on the number of items present
the printItems functions is called recursively to generate the remaining docs. printItems in turn calls
the printElement and printText to print the respective elements. printItems also tries its best to fill
the content completely according to the width specified. Finally after all the internal elements are
generated, the root tag is closed and group is applied so that the best layout is chosen. The ugly aspect
of the document generated is that it has some indentation issues.

-}



main :: IO ()
main = do
        content <- getContents
        args <- getArgs
        let width = if length args == 1 && isInt (head args)
                     then read (head args)
                     else 60
        case parseMaybe sxmlP (join $ lines content) of
            Just doc -> pprint width (sxmlD doc)
            Nothing  -> putStrLn "** PARSE ERROR **"
        return ()


isInt :: String -> Bool
isInt = foldr(\x acc -> isDigit x && acc) True
{-

$ runhaskell SXMLReformat < gettysburg.sxml

$ runhaskell sXMLReformat 40 < gettysburg.sxml

-}
