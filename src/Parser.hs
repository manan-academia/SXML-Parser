-- Monadic Parser Combinators
-- Prof. Matthew Fluet

{-# OPTIONS -Wall #-}

{-
References::

Graham Hutton and Erik Meijer.  (1996) "Monadic Parser Combinators".
Technical Report, Department of Computer Science, University of
Nottingham (NOTTCS-TR-96-4).

Graham Hutton.  (2007) _Programming in Haskell_ (Chapter 8: A
Functional Parser).

Text.ParserCombinators.ReadP
https://downloads.haskell.org/~ghc/8.0.2/docs/html/libraries/base-4.9.1.0/Text-ParserCombinators-ReadP.html

-}

module Parser (
  Parser {- instance Functor, Applicative, Monad -},
  parse, parseMaybe,
  get, look,
  pfail, (+++), (<++),
  pfilter, satisfy,
  char, string,
  digit, lower, upper, space, letter, alphanum,
  choice, first,
  manyA, many1A, skipManyA, skipMany1A, sepByA, sepBy1A, chainrA, chainr1A, chainlA, chainl1A,
  manyL, many1L, skipManyL, skipMany1L, sepByL, sepBy1L, chainrL, chainr1L, chainlL, chainl1L,
  skipSpaces,
  between, token,
  ioption, imaybe, xoption, xmaybe,
  count,
  natural, integer
) where

import           Data.Char

-- Core --

{-
A parser for things
Is a function from strings
To lists of pairs
Of things and strings
                Graham Hutton
                (w/ apologies to Dr. Seuss)
-}
newtype Parser a = Parser (String -> [(a,String)])


parse :: Parser a -> String -> [(a,String)]
parse (Parser p) = p


-- Useful external function;
-- succeeds if entire input can be parsed in exactly one way.
parseMaybe :: Parser a -> String -> Maybe a
parseMaybe p s = case [ x | (x,[]) <- parse p s ] of
                   [x] -> Just x
                   _   -> Nothing


-- Consumes and returns the next character. Fails if there is no input left.
-- Primitive action.
get :: Parser Char
get = Parser (\ s -> case s of
                       []   -> []
                       c:cs -> [(c,cs)])

-- Look-ahead: returns the part of the input that is left, without consuming it.
-- Primitive action.
look :: Parser String
look = Parser (\ s -> [(s,s)])


-- Always succeeds; monadic return.
prtrn :: a -> Parser a
prtrn x = Parser (\ s -> [(x,s)])

-- Sequencing; monadic bind.
pbind :: Parser a -> (a -> Parser b) -> Parser b
pbind p1 f = Parser (\ s0 -> let -- parse according to p1
                                 {- rs1 :: [(a, String)] -}
                                 rs1 = parse p1 s0
                                 -- for each p1 parse,
                                 -- parse according to f applied to
                                 -- the p1 parse result and the p1 parse remainder
                                 {- rss2 :: [[(b, String)]] -}
                                 rss2 = map (\ (x, s1) -> parse (f x) s1) rs1
                                 -- flatten list of p2 parses
                             in  concat rss2)
{-
Equivalent definitions:
pbind p1 f = Parser (\ s0 -> concatMap (\ (x, s1) -> parser (f x) s1) (parse p1 s0))
pbind p1 f = Parser (\ s0 -> [ (y, s2) | (x, s1) <- parse p1 s0, (y, s2) <- parser (f x) s1 ])
-}

-- Apply a pure function to parse result; functorial mapping.
pfmap :: (a -> b) -> Parser a -> Parser b
pfmap f p = Parser (\ s0 -> let -- parse according to p1
                                {- rs1 :: [(a, String)] -}
                                rs1 = parse p s0
                                -- for each p1 parse,
                                -- apply f to the p1 parse result
                                {- rs2 :: [(b, String)] -}
                                rs2 = map (\ (x, s1) -> (f x, s1)) rs1
                            in  rs2)
{-
Equivalent definitions:
pfmap f p = Parser (\ s0 -> map (\ (x, s1) -> (f x, s1)) (parse p1 s0))
pfmap f p = Parser (\ s0 -> [ (f x, s1) | (x, s1) <- parse p1 s0 ])
pfmap f p = p `pbind` \ x -> prtrn (f x)
pfmap f p = do x <- p; return (f x)
-}

instance Monad Parser where
  return = prtrn
  (>>=) = pbind
instance Applicative Parser where
  pure = prtrn
  pf <*> px = do f <- pf ; x <- px; return (f x)
instance Functor Parser where
  fmap = pfmap

-- Always fails.
pfail :: Parser a
pfail = Parser (const [])


infixr 5 +++, <++

-- Symmetric choice.
(+++) :: Parser a -> Parser a -> Parser a
p1 +++ p2 = Parser (\ s -> parse p1 s ++ parse p2 s)

-- Left-biased choice
(<++) :: Parser a -> Parser a -> Parser a
p1 <++ p2 = Parser (\ s -> case parse p1 s of
                             [] -> parse p2 s
                             rs -> rs)


-- Derived --

-- Consumes and returns a parsed value, if it satisfies the specified predicate.
pfilter :: (a -> Bool) -> Parser a -> Parser a
pfilter f p = do x <- p
                 if f x
                   then return x
                   else pfail

-- Consumes and returns the next character, if it satisfies the specified predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = pfilter f get

-- Parses and returns the specified character.
char :: Char -> Parser Char
char c = satisfy (==c)

-- Parses and returns particular kinds of characters
digit :: Parser Char
digit = satisfy isDigit
lower :: Parser Char
lower = satisfy isLower
upper :: Parser Char
upper = satisfy isUpper
space :: Parser Char
space = satisfy isSpace

letter :: Parser Char
letter = lower +++ upper

alphanum :: Parser Char
alphanum = letter +++ digit

-- Parses and returns the specified string.
string :: String -> Parser String
{-
string []     = return ""
string (c:cs) = do c' <- char c
                   cs' <- string cs
                   return (c':cs')
-}
string s = loop s >> return s
  where loop []     = return ()
        loop (c:cs) = char c >> loop cs
{- Why is the second definition more efficient? -}


-- Combines all parsers in the specified list.
choice :: [Parser a] -> Parser a
choice = foldl (+++) pfail

-- Parses according to the first succeeding parser in the specified list.
first :: [Parser a] -> Parser a
first = foldl (<++) pfail


-- Parses zero or more occurrences of the given parser.
manyA :: Parser a -> Parser [a]
manyA p = return [] +++ many1A p

-- Parses one or more occurrences of the given parser.
many1A :: Parser a -> Parser [a]
many1A p = do x <- p
              xs <- manyA p
              return (x : xs)

-- Like `manyA`, but discards the result.
skipManyA :: Parser a -> Parser ()
skipManyA p = manyA p >> return ()

-- Like `many1A`, but discards the result.
skipMany1A :: Parser a -> Parser ()
skipMany1A p = many1A p >> return ()

-- `sepByA p sep` parses zero or more occurrences of `p`, separated by `sep`.
-- Returns a list of values returned by `p`.
sepByA :: Parser a -> Parser sep -> Parser [a]
sepByA p sep = return [] +++ sepBy1A p sep

-- `sepBy1A p sep` parses one or more occurrences of `p`, separated by `sep`.
-- Returns a list of values returned by `p`.
sepBy1A :: Parser a -> Parser sep -> Parser [a]
sepBy1A p sep = do x <- p
                   xs <- manyL (sep >> p)
                   return (x : xs)

-- `chainrA p op x` parses zero or more occurrences of `p`, separated by `op`.
-- Returns a value produced by a *right associative* application of all functions returned by `op`.
-- If there are no occurrences of `p`, `x` is returned.
chainrA :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainrA p op x = return x +++ chainr1A p op

-- Like `chainrA`, but parses one or more occurrences of `p`.
chainr1A :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1A p op = p >>= rest
  where rest x = return x +++ (do f <- op
                                  y <- p >>= rest
                                  return (x `f` y))


-- `chainlA p op x` parses zero or more occurrences of `p`, separated by `op`.
-- Returns a value produced by a *left associative* application of all functions returned by `op`.
-- If there are no occurrences of `p`, `x` is returned.
chainlA :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainlA p op x = return x +++ chainl1A p op

-- Like `chainlA`, but parses one or more occurrences of `p`.
chainl1A :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1A p op = p >>= rest
  where rest x = return x +++ (do f <- op
                                  y <- p
                                  rest (x `f` y))


-- Parses only the longest (empty or non-empty) occurrences of the given parser.
manyL :: Parser a -> Parser [a]
manyL p = many1L p <++ return []

-- Parses only the longest (non-empty) occurrences of the given parser.
many1L :: Parser a -> Parser [a]
many1L p = (:) <$> p <*> manyL p
{-
Equivalent definition:
many1L p = do x <- p
              xs <- manyL p
              return (x : xs)
-}

-- Like `manyL`, but discards the result.
skipManyL :: Parser a -> Parser ()
skipManyL p = manyL p >> return ()

-- Like `many1L`, but discards the result.
skipMany1L :: Parser a -> Parser ()
skipMany1L p = many1L p >> return ()

-- `sepByL p sep` parses only the longest (empty or non-empty) occurrences of `p`, separated by `sep`.
-- Returns a list of values returned by `p`.
sepByL :: Parser a -> Parser sep -> Parser [a]
sepByL p sep = sepBy1L p sep <++ return []

-- `sepBy1L p sep` parses only the longest (non-empty) occurrences of `p`, separated by `sep`.
-- Returns a list of values returned by `p`.
sepBy1L :: Parser a -> Parser sep -> Parser [a]
sepBy1L p sep = (:) <$> p <*> manyL (sep >> p)
{-
Equivalent definition:
sepBy1L p sep = do x <- p
                   xs <- manyL (sep >> p)
                   return (x : xs)
-}

-- `chainrL p op x` parses only the longest (empty or non-empty) occurrences of `p`, separated by `op`.
-- Returns a value produced by a *right associative* application of all functions returned by `op`.
-- If there are no occurrences of `p`, `x` is returned.
chainrL :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainrL p op x = chainr1L p op <++ return x

-- Like `chainrL`, but parses only the longest (non-empty) occurrences of `p`.
chainr1L :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1L p op = p >>= rest
  where rest x = (do f <- op
                     y <- p >>= rest
                     return (x `f` y)) <++ return x


-- `chainlL p op x` parses only the longest (empty or non-empty) occurrences of `p`, separated by `op`.
-- Returns a value produced by a *left associative* application of all functions returned by `op`.
-- If there are no occurrences of `p`, `x` is returned.
chainlL :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainlL p op x = chainl1L p op <++ return x

-- Like `chainlL`, but parses only the longest (non-empty) occurrences of `p`.
chainl1L :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1L p op = p >>= rest
  where rest x = (do f <- op
                     y <- p
                     rest (x `f` y)) <++ return x


{-
Why are the `*L` parsers typically preferred to the `*A` parsers?
-}

-- Skips all whitespace.
skipSpaces :: Parser ()
skipSpaces = skipManyL space


-- `between open close p` parses `open`, followed by `p` and finally `close`.
-- Only the value of `p` is returned.
between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do _ <- open
                          x <- p
                          _ <- close
                          return x

token :: Parser a -> Parser a
token = between skipSpaces skipSpaces


-- `ioption x p` will (inclusively) parse `p` or return `x` without consuming any input.
ioption :: a -> Parser a -> Parser a
ioption x p = p +++ return x

-- `imaybe p` will try to parse `p`.
imaybe :: Parser a -> Parser (Maybe a)
imaybe p = ioption Nothing (Just <$> p)

-- `xoption x p` will (exclusively) parse `p` or return `x` without consuming any input.
xoption :: a -> Parser a -> Parser a
xoption x p = p <++ return x

-- `xmaybe p` will try to parse `p`.
xmaybe :: Parser a -> Parser (Maybe a)
xmaybe p = xoption Nothing (Just <$> p)


-- `count n p` parses `n` occurrences of `p` in sequence.
count :: Int -> Parser a -> Parser [a]
count 0 _ = return []
count n p = (:) <$> p <*> count (pred n) p


-- Parse a natural number
natural :: Parser Integer
natural = idigit >>= aux
  where idigit :: Parser Integer
        idigit = (\ c -> fromIntegral (ord c - ord '0' )) <$> digit
        aux :: Integer -> Parser Integer
        aux n = xoption n (idigit >>= \ d -> aux (10*n + d))

          -- Parse a (signed) integer
integer :: Parser Integer
integer = sign <*> natural
  where sign :: Parser (Integer -> Integer)
        sign = xoption id (const (\ x -> -x) <$> char '-')
