module SExpr where

import           Parser
import           PrettyPrint

data SExpr = Symbol String | Number Integer | List [SExpr]
  deriving (Eq, Read, Show)


symbolP :: Parser SExpr
symbolP = Symbol <$> token (many1L lower)

numberP :: Parser SExpr
numberP = Number <$> token integer

listP :: Parser SExpr
listP = List <$> between (token $ char '(') (token $ char ')') (manyL sexprP)

sexprP :: Parser SExpr
sexprP = choice [symbolP, numberP, listP]


sexprD :: SExpr -> Doc
sexprD (Symbol s) = text s
sexprD (Number i) = text (show i)
sexprD (List ss)  = text "(" <+> nest 1 (sep (sexprD <$> ss)) <+> slbreak <+> text ")"
