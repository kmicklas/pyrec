module Pyrec.Parse where

import Control.Applicative hiding (many, (<|>))

import Text.Parsec
import Text.Parsec.String

import Pyrec.AST as A

type Parse = Parsec String Bool -- Bool is for after space or not

-- Parser:

parseString :: Parse a -> String -> Either ParseError a
parseString p input = runParser p False "test" input

program :: Parse Module
program = endToken >> Module <$> provide <*> many import_ <*> block

provide :: Parse (Node Provide)
provide = node $ option NoProvide $
            do kw "provide"
               (op "*" >> return ProvideAll) <|> (ProvideExpr <$> expr)

import_ :: Parse (Node Import)
import_ = node $ do kw "import"
                    name <- Named <$> iden
                    option (Import name) $
                      kw "as" >> ImportQualified name <$> iden

block :: Parse Block
block = many stmt

stmt :: Parse (Node Statement)
stmt = node $ try (fmap LetStmt $ Let <$> (bind <* op "=") <*> expr) <|>
              (ExprStmt <$> expr)

bind :: Parse Bind
bind = Bind <$> iden <*> optionMaybe type_

type_ :: Parse (Node Type)
type_ = node $ TId <$> iden

expr :: Parse (Node Expr)
expr = node $ Num <$> number

none :: Parse ()
none = return ()

-- Lexer:

node :: Parse a -> Parse (Node a)
node p = Node <$> getPosition <*> p

number :: Parse Double
number = read <$> many1 digit

iden :: Parse Id
iden = tok "identifier" $ node $
         do word <- (:) <$> idenStart <*> many idenChar
                        <* notFollowedBy idenChar
            if elem word keywords then parserZero else return word

kw :: String -> Parse ()
kw word = tok word $ string word >> notFollowedBy idenChar

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

op word = tok word $ string word >> notFollowedBy operatorChar

operators = [ "+", "-", "*", "/"
            , "<=", ">=", "==", "<>", "<", ">"
            ]

operatorChar :: Parse Char
operatorChar = oneOf "+-*/<>="

endToken :: Parse ()
endToken = skipMany $ ((space >> none) <|>
                       (char '#' >> manyTill anyChar newline >> none))
                      >> putState True

tok :: String -> Parse a -> Parse a
tok name p = lookAhead p *> putState False *> p <* endToken <?> name

parenSpace :: Bool -> Parse ()
parenSpace s = do afterSpace <- getState
                  if s == afterSpace
                  then char '(' >> putState False
                  else parserZero

parenWithSpace = parenSpace True
parenNoSpace = parenSpace False

paren :: Parse ()
paren = char '(' >> putState False >> endToken
