module Pyrec.Parse where

import Control.Applicative hiding (many, (<|>))

import Text.Parsec
import Text.Parsec.String

import qualified Pyrec.AST as A
import Pyrec.AST.Parse
import Pyrec.AST.Module

type Parse = Parsec String Bool -- Bool is for after space or not

parseString :: Parse a -> String -> Either ParseError a
parseString p input = runParser p False "test" input

program :: Parse (Module Expr)
program = endToken >> Module <$> provide <*> many import_ <*> block

provide :: Parse (Provide Expr)
provide = undefined
import_ = undefined
block = undefined

iden :: Parse String
iden = tok $ do word <- (:) <$> idenStart <*> many idenChar
                            <* notFollowedBy idenChar
                if elem word keywords then parserZero else return word

kw :: String -> Parse ()
kw word = tok $ string word >> notFollowedBy idenChar

keywords = [ "import", "provide", "as"
           , "var"
           , "fun", "method", "doc:"
           , "where:"
           , "check:"
           , "try:", "except"
           , "cases"
           , "when", "if:", "then:", "if", "else if", "else:"
           , "data", "with:", "sharing:", "mutable", "cyclic"
           , "datatype", "with constructor"
           , "graph:", "block:"
           , "for", "from"
           , "end", ";"
           , "and", "or", "not", "is", "raises"
           ]

idenStart :: Parse Char
idenStart = letter

idenChar :: Parse Char
idenChar = alphaNum <|> char '-'

op word = tok $ string word >> notFollowedBy operatorChar

operators = [ "+", "-", "*", "/"
            , "<=", ">=", "==", "<>", "<", ">"
            ]

operatorChar :: Parse Char
operatorChar = oneOf "+-*/<>="

endToken :: Parse ()
endToken = skipMany $ ((space >> none) <|>
                       (char '#' >> manyTill anyChar newline >> none))
                      >> putState True

tok :: Parse a -> Parse a
tok p = lookAhead p *> putState False *> p <* endToken

parenSpace :: Bool -> Parse ()
parenSpace s = do afterSpace <- getState
                  if s == afterSpace
                  then char '(' >> putState False
                  else parserZero

parenWithSpace = parenSpace True
parenNoSpace = parenSpace False

paren :: Parse ()
paren = char '(' >> putState False >> endToken

none :: Parse ()
none = return ()
