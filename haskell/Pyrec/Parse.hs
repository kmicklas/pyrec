module Pyrec.Parse where

import Control.Applicative hiding (many, (<|>))

import Text.Parsec
import Text.Parsec.String

import Pyrec.AST as A

type Parse = Parsec String Bool -- Bool is for after space or not

-- Parser:

parseString :: Parse a -> String -> Either ParseError a
parseString p input = runParser p True "test" input

program :: Parse Module
program = endToken >> Module <$> provide <*> many import_ <*> block
                   <* spaces <* eof

provide :: Parse (Node Provide)
provide = node $ option NoProvide $
            do kw "provide"
               (op "*" >> return ProvideAll)
                 <|> (ProvideExpr <$> expr <* end)

import_ :: Parse (Node Import)
import_ = node $ do kw "import"
                    name <- Named <$> iden
                    option (Import name) $
                      kw "as" >> ImportQualified name <$> iden

block :: Parse Block
block = Statements <$> many stmt

stmt :: Parse (Node Statement)
stmt = node $ try letStmt
          <|> try varStmt
          <|> funStmt -- no try because we do it internally
          <|> try assignStmt
          <|> (ExprStmt <$> expr)

letStmt :: Parse Statement
letStmt = LetStmt <$> let_

varStmt :: Parse Statement
varStmt = kw "var" *> (VarStmt <$> let_)

let_ :: Parse (Let Id)
let_ = Let <$> idenBind <* op "=" <*> expr

assignStmt :: Parse Statement
assignStmt = AssignStmt <$> iden <* op ":=" <*> expr

funStmt :: Parse Statement
funStmt = try ((kw "fun" *>) $
                  FunStmt <$> optionMaybe typeParams)
                    <*> iden
                    <*> optionMaybe params
                    <*> optionMaybe (op "->" *> type_)
                    <* begin
                    <*> block
                    <* end

typeParams :: Parse [Id]
typeParams = angleNoSpace *> sepBy iden (op ",") <* bracket '>'

params :: Parse [Bind Id]
params = parenNoSpace *> sepBy idenBind (op ",") <* bracket ')'

bind :: (Parse a) -> Parse (Bind a)
bind b = Bind <$> b <*> optionMaybe (op "::" *> type_)

idenBind = bind iden
keyBind  = bind key

type_ :: Parse (Node Type)
type_ = node $ TIdent <$> iden

expr :: Parse (Node Expr)
expr = appVal

appVal :: Parse (Node Expr)
appVal = do vn@(Node p v) <- val
            foldl (combine p) vn <$> many args
  where combine p f (Right vargs) = Node p $ App  f vargs
        combine p f (Left  targs) = Node p $ AppT f targs
        args = vapp <|> tapp
        vapp = Right <$> (parenNoSpace *> sepBy expr  (op ",") <* bracket ')')
        tapp = Left  <$> (angleNoSpace *> sepBy type_ (op ",") <* bracket '>')

val :: Parse (Node Expr)
val = node $ Num <$> number <|>
             Ident  <$> iden <|>
             funExpr <|>
             parenExpr <|>
             objExpr

funExpr :: Parse Expr
funExpr = (kw "fun" *>) $
            Fun <$> optionMaybe typeParams
                <*> optionMaybe params
                <*> optionMaybe (op "->" *> type_)
                <* begin
                <*> block
                <* end

parenExpr :: Parse Expr
parenExpr = do parenWithSpace
               en@(Node _ e) <- expr
               option e $ TypeConstraint en <$> (op "::" *> type_)
            <* bracket ')'

objExpr :: Parse Expr
objExpr = bracket '{' *> (Obj <$> sepBy objField (op ",")) <* bracket '}'

objField :: Parse (Node Field)
objField = node $ option Immut (kw "mutable" *> pure Mut)
                    <*> (Let <$> keyBind <* op ":" <*> expr)

key :: Parse Key
key = (Name <$> iden) <|> (bracket '[' *> (Dynamic <$> expr) <* bracket ']')

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
operatorChar = oneOf "+-*/<>=:"

endToken :: Parse ()
endToken = skipMany $ (<?> "whitspace") $
             ((space >> none) <|>
              (char '#' *> manyTill anyChar newline *> none))
                *> putState True

tok :: String -> Parse a -> Parse a
tok name p = try $ (lookAhead p
                      *> putState False
                      *> p
                      <* endToken
                      <?> name)

opSpace :: Char -> Bool -> Parse ()
opSpace c s = do afterSpace <- getState
                 if s == afterSpace
                 then char c >> putState False
                 else parserZero
                 endToken

parenWithSpace = opSpace '(' True
parenNoSpace = opSpace '(' False

angleWithSpace = opSpace '<' True
angleNoSpace = opSpace '<' False

bracket :: Char -> Parse ()
bracket c = char c >> putState False >> endToken
