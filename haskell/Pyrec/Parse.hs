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
                   <* spaces <* eof

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
stmt = node $ try letStmt
          <|> try varStmt
          <|> try funStmt
          <|> (ExprStmt <$> expr)

letStmt :: Parse Statement
letStmt = LetStmt <$> letBind

varStmt :: Parse Statement
varStmt = kw "var" *> (VarStmt <$> letBind)

letBind :: Parse Let
letBind = Let <$> bind <* op "=" <*> expr

funStmt :: Parse Statement
funStmt = (kw "fun" *>) $
            FunStmt <$> optionMaybe typeParams
                    <*> iden
                    <*> params
                    <*> optionMaybe (op "->" *> type_)
                    <* begin
                    <*> block
                    <* end

typeParams :: Parse [Id]
typeParams = angleNoSpace *> sepBy iden (op ",") <* op ">"

params :: Parse [Bind]
params = parenNoSpace *> sepBy bind (op ",") <* closeParen

bind :: Parse Bind
bind = Bind <$> iden <*> optionMaybe (op "::" *> type_)

type_ :: Parse (Node Type)
type_ = node $ TId <$> iden

expr :: Parse (Node Expr)
expr = appVal

appVal :: Parse (Node Expr)
appVal = do vn@(Node p v) <- val
            foldl (combine p) vn <$> many args
  where combine p f (Right vargs) = Node p $ App  f vargs
        combine p f (Left  targs) = Node p $ TApp f targs
        args = vapp <|> tapp
        vapp = Right <$> (parenNoSpace *> sepBy expr  (op ",") <* closeParen)
        tapp = Left  <$> (angleNoSpace *> sepBy type_ (op ",") <* closeAngle)

val :: Parse (Node Expr)
val = node $ Num <$> number <|>
             Id  <$> iden <|>
             funExpr

funExpr :: Parse Expr
funExpr = (kw "fun" *>) $
            Fun <$> optionMaybe typeParams
                <*> optionMaybe params
                <*> optionMaybe (op "->" *> type_)
                <* begin
                <*> block
                <* end

begin :: Parse ()
begin = op ":"

end :: Parse ()
end = kw "end" <|> op ";"

none :: Parse ()
none = return ()

-- Lexer:

node :: Parse a -> Parse (Node a)
node p = Node <$> getPosition <*> p

number :: Parse Double
number = tok "number" $ read <$> many1 digit

iden :: Parse Id
iden = tok "identifier" $ node $ try $
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
endToken = skipMany $ (<?> "whitspace") $
             ((space >> none) <|>
              (char '#' *> manyTill anyChar newline *> none))
                *> putState True

tok :: String -> Parse a -> Parse a
tok name p = lookAhead p *> putState False *> p <* endToken <?> name

opSpace :: Char -> Bool -> Parse ()
opSpace c s = do afterSpace <- getState
                 if s == afterSpace
                 then char c >> putState False
                 else parserZero

parenWithSpace = opSpace '(' True
parenNoSpace = opSpace '(' False

angleWithSpace = opSpace '<' True
angleNoSpace = opSpace '<' False

bracket :: Char -> Parse ()
bracket c = char c >> putState False >> endToken

openParen  = bracket '('
closeParen = bracket ')'
openAngle  = bracket '<'
closeAngle = bracket '>'
