-- Monadic Parsing and Monoidal Pretty Printing Combinators

-- Names:

-- Submit to the myCourses Recitation06 DropBox by 10:59am.

{-# OPTIONS -Wall -Wno-unused-imports #-}

{-

"JSON (JavaScript Object Notation) is a lightweight data-interchange format. It
is easy for humans to read and write. It is easy for machines to parse and
generate."  (www.json.org)

In this exercise, you will validate the second claim ("It is easy for machines
to parse and generate.") by writing a parser and pretty printer for JSON values.

Briefly, a JSON value can be an *object*, *array*, *number*, *string*, `true`,
`false`, or `null`.

An object is an unordered set of name/value pairs.  An object begins with `{`
and ends with `}`; name/value pairs are separated by `,`; a name and its
corresponding value are separated by `:`; a name is a *string*.

An array is an ordered collection of values.  An array begins with `[` and ends
with `]`; values are separated by `,`.

For the purposes of this exercise, a number is a signed decimal integer number.
(True JSON allows decimal floating-point numbers).

For the purposes of this exercise, a string is sequence of characters that
begins and ends with '"' and uses (a simple set of) backslash escape sequences.
(True JSON allows hexadecimal escape sequences.)

Whitespace can be inserted between any pair of tokens.

For more details, refer to www.json.org and/or ECMA-404 The JSON Data
Interchange Standard
(http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf).

HINT: Use `integer :: Parser Integer` from the `Parser` module for JSON numbers.
HINT: Use `strlit :: Parser String` from this module for JSON strings.

For a fully-featured Haskell implementation of JSON, see the `Text.JSON` package
(http://hackage.haskell.org/package/json).
-}

module JSON where

import           Data.Char

import           Parser
import           PrettyPrint


-- Parse a JSON string literal
-- Slightly simplified; does not handle hexadecimal escape sequences.
strlit :: Parser String
strlit = between (char '"') (char '"') (manyL aux)
  where aux = satisfy (\ c -> not (c == '"' || c == '\\' || isControl c))
              +++
              (char '\\' >> choice (esc <$> [('"', '"'),
                                             ('\\', '\\'),
                                             ('/', '/'),
                                             ('b', '\b'),
                                             ('f', '\f'),
                                             ('n', '\n'),
                                             ('r', '\r'),
                                             ('t', '\t')]))
        esc (c, c') = char c >> return c'


data JValue = JObject [(String,JValue)] | JArray [JValue]
            | JString String | JNumber Integer
            | JBool Bool | JNull
  deriving (Eq, Show, Read)


jvalueP :: Parser JValue
jvalueP = choice [jobjectP, jarrayP, jnumberP, jstringP, jboolP, jnullP]

jnullP :: Parser JValue
jnullP = token (string "null") >> return JNull

jboolP :: Parser JValue
jboolP = JBool <$> token ((string "true" >> return True) +++ (string "false" >> return False))

jstringP :: Parser JValue
jstringP = JString <$> strlit

jnumberP :: Parser JValue
jnumberP = JNumber <$> token integer

jarrayP :: Parser JValue
jarrayP = JArray <$> between (token (char '[')) (token (char ']')) (sepByL jvalueP (token (char ',')))

jobjectP :: Parser JValue
jobjectP = JObject <$> between (token (char '{')) (token (char '}')) (sepByL nvP (token (char ',')))
  where nvP = do n <- strlit;
                 _ <- token (char ':')
                 v <- jvalueP
                 return (n, v)


jvalueD :: JValue -> Doc
jvalueD (JObject nvs) = text "{" <+> nest 1 (vsep (punctuate (text ",") (auxD <$> nvs)) <+> text "}")
  where auxD (n,v) = text (show n) <+> text ": " <+> nest (length n + 4) (jvalueD v)
jvalueD (JArray vs)   = text "[" <+> nest 1 ((if all atomic vs then fillSep else vsep) (punctuate (text ",") (jvalueD <$> vs)) <+> text "]")
  where atomic (JObject []) = True
        atomic (JObject _)  = False
        atomic (JArray [])  = True
        atomic (JArray _)   = False
        atomic (JString _)  = True
        atomic (JNumber _)  = True
        atomic (JBool _)    = True
        atomic JNull        = True
jvalueD (JString s)   = text (show s)
jvalueD (JNumber n)   = text (show n)
jvalueD (JBool True)  = text "true"
jvalueD (JBool False) = text "false"
jvalueD JNull         = text "null"


main :: IO ()
main = case parseMaybe jvalueP jsonText1 of
         Nothing -> putStrLn "** PARSE ERROR **"
         Just jv -> pprint 40 (jvalueD jv)

{-
Reference solution output:

{"firstName": "John",
 "lastName": "Smith",
 "isAlive": true,
 "age": 27,
 "address": {"streetAddress": "21 2nd Street",
             "city": "New York",
             "state": "NY",
             "postalCode": "10021-3100"},
 "phoneNumbers": [{"type": "home",
                   "number": "212 555-1234"},
                  {"type": "office",
                   "number": "646 555-4567"},
                  {"type": "mobile",
                   "number": "123 456-7890"}],
 "children": [],
 "spouse": null,
 "lucky_numbers": [2, 3, 5, 7, 11, 13,
                   17, 19, 23, 29, 31,
                   37, 41, 43, 47, 53,
                   59, 61, 67, 71, 73,
                   79, 83, 89, 97, 101,
                   103, 107, 109, 113,
                   127, 131, 137, 139,
                   149, 151, 157, 163,
                   167, 173, 179, 181,
                   191, 193, 197, 199,
                   211, 223, 227, 229,
                   233, 239, 241, 251,
                   257, 263, 269, 271]}
-}


jsonText1 :: String
jsonText1 =
  "{" ++
  "  \"firstName\": \"John\"," ++
  "  \"lastName\": \"Smith\"," ++
  "  \"isAlive\": true," ++
  "  \"age\": 27" ++
  "}"
  {-}"  \"address\": {" ++
  "    \"streetAddress\": \"21 2nd Street\"," ++
  "    \"city\": \"New York\"," ++
  "    \"state\": \"NY\"," ++
  "    \"postalCode\": \"10021-3100\"" ++
  "  }," ++
  "  \"phoneNumbers\": [" ++
  "    {" ++
  "      \"type\": \"home\"," ++
  "      \"number\": \"212 555-1234\"" ++
  "    }," ++
  "    {" ++
  "      \"type\": \"office\"," ++
  "      \"number\": \"646 555-4567\"" ++
  "    }," ++
  "    {" ++
  "      \"type\": \"mobile\"," ++
  "      \"number\": \"123 456-7890\"" ++
  "    }" ++
  "  ]," ++
  "  \"children\": []," ++
  "  \"spouse\": null," ++
  "  \"lucky_numbers\": [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271]" ++
  "}"-}
