module Pyrec.Parse where

import Control.Applicative hiding (many, (<|>))

import Text.Parsec
import Text.Parsec.String

import Pyrec.Lex
import Pyrec.Misc
import Pyrec.AST           as A

type Parse = Parsec [Token] ()

-- Parser:

parseString :: Parse a -> String -> Either ParseError a
parseString p input = runParser p () "test" $ scan "test" input

program :: Parse Module
program = Module <$> provide <*> many import_ <*> block <* eof

provide :: Parse (Node Provide)
provide = node $ option NoProvide $
            do kw "provide"
               (kw "*" >> return ProvideAll)
                 <|> (ProvideExpr <$> expr <* end)

import_ :: Parse (Node Import)
import_ = node $ do kw "import"
                    name <- Named <$> iden
                    option (Import name) $
                      kw "as" >> ImportQualified name <$> iden

block :: Parse Block
block = Statements <$> many stmt

stmt :: Parse (Node Statement)
stmt = node $  try letStmt
           <|> try varStmt
           <|> funStmt -- no try because we do it internally
           <|> try assignStmt
           <|> (ExprStmt <$> expr)

letStmt :: Parse Statement
letStmt = LetStmt <$> let_

varStmt :: Parse Statement
varStmt = kw "var" *> (VarStmt <$> let_)

let_ :: Parse (Let Id)
let_ = Let <$> idenBind <* kw "=" <*> expr

assignStmt :: Parse Statement
assignStmt = AssignStmt <$> iden <* kw ":=" <*> expr

funStmt :: Parse Statement
funStmt = try ((kw "fun" *>) $
                  FunStmt <$> optionMaybe typeParams)
                    <*> iden
                    <*> optionMaybe params
                    <*> optionMaybe (kw "->" *> type_)
                    <* begin
                    <*> block
                    <* end

typeParams :: Parse [Id]
typeParams = angleNoSpace *> sepBy iden (kw ",") <* close '>'

params :: Parse [Bind Id]
params = parenNoSpace *> sepBy idenBind (kw ",") <* close ')'

bind :: (Parse a) -> Parse (Bind a)
bind b = Bind <$> b <*> optionMaybe (kw "::" *> type_)

idenBind = bind iden
keyBind  = bind key

type_ :: Parse (Node Type)
type_ = node $ TIdent <$> iden

expr :: Parse (Node Expr)
expr = do first@(Node l _) <- appVal
          rest <- many $ (,) <$> choice binOps <*> appVal
          if null rest
          then return first
          else return $ Node l $ BinOp first rest

pfoldl :: Parse a -> Parse (a -> a) -> Parse a
pfoldl p f = foldl (flip ($)) <$> p <*> many f

appVal :: Parse (Node Expr)
appVal = pfoldl val args
  where args = vapp <|> tapp <|> dot
        parseArgs con arg = do a <- arg
                               return $ \ f@(Node l _) -> Node l $ con f a
        vapp = parseArgs App $ parenNoSpace
                                 *> sepBy expr  (kw ",")
                                 <* close ')'
        tapp = parseArgs AppT $ angleNoSpace
                                  *> sepBy type_ (kw ",")
                                  <* close '>'
        dot  = parseArgs Dot $ kw "." *> key

val :: Parse (Node Expr)
val = node $  Ident <$> iden
          <|> Num   <$> num
          <|> Str   <$> str
          <|> funExpr
          <|> parenExpr
          <|> objExpr
          <|> ifExpr
          <|> casesExpr

funExpr :: Parse Expr
funExpr = (kw "fun" *>) $
            Fun <$> optionMaybe typeParams
                <*> optionMaybe params
                <*> optionMaybe (kw "->" *> type_)
                <* begin
                <*> block
                <* end

parenExpr :: Parse Expr
parenExpr = do parenWithSpace
               en@(Node _ e) <- expr
               option e $ TypeConstraint en <$> (kw "::" *> type_)
            <* close ')'

objExpr :: Parse Expr
objExpr = open '{' *> (Obj <$> sepBy objField (kw ",")) <* close '}'

objField :: Parse (Node Field)
objField = node $ option Immut (kw "mutable" *> pure Mut)
                    <*> (Let <$> keyBind <* kw ":" <*> expr)

key :: Parse Key
key = (Name <$> iden) <|> (open '[' *> (Dynamic <$> expr) <* close ']')

ifExpr :: Parse Expr
ifExpr = (kw "if" *>) $ If <$> ((:) <$> branch
                                    <*> (many $ try (kw "else")
                                                  *> kw "if"
                                                  *> branch))
                           <*> optionMaybe (kw "else" *> begin *> block)
                           <* end

branch :: Parse Branch
branch = Branch <$> expr <* begin <*> block

casesExpr :: Parse Expr
casesExpr = (kw "cases" *>) $ Cases
              <$> (optionMaybe $ parenNoSpace *> type_ <* close ')')
              <*> expr
              <* begin
              <*> many case_
              <* end

case_ :: Parse Case
case_ = (kw "|" *>) $ (pure Else <* kw "else" <* kw "=>" <*> block) <|>
                      (pure Case <*> iden
                                 <*> optionMaybe params
                                 <* kw "=>"
                                 <*> block)

begin :: Parse ()
begin = kw ":"

end :: Parse ()
end = kw "end" <|> kw ";"

bkw :: (String -> Parse ()) -> String -> Parse String
bkw kind word = kind word *> pure word

binOps = [ bkw kw "+", bkw kw "-", bkw kw "*", bkw kw "/"
         , bkw kw "<=", bkw kw ">=", bkw kw "=="
         , bkw kw "<>", bkw kw "<", bkw kw ">"
         , bkw kw "and", bkw kw "or"
         ]

none :: Parse ()
none = return ()

-- Lexer interaction:

node :: Parse a -> Parse (Node a)
node p = Node <$> getPosition <*> p

tok :: (Tok -> Maybe a) -> Parse a
tok match = tokenPrim show nextPos $ \ (Token _ t) -> match t
  where nextPos _ _ (Token p _ : _) = p
        nextPos p _ []              = p

num :: Parse Double
num = tok $ \case TNum n -> Just n
                  _      -> Nothing

str :: Parse String
str = tok $ \case TStr i -> Just i
                  _      -> Nothing

iden :: Parse Id
iden = node $ tok $ \case Iden i -> Just i
                          _      -> Nothing

kw :: String -> Parse ()
kw w = tok $ \case Kw w -> Just ()
                   _    -> Nothing

keywords = [ "import", "provide", "as"
           , "var"
           , "fun", "method", "doc"
           , "where"
           , "check"
           , "try", "except"
           , "cases"
           , "when", "if", "then", "else"
           , "data", "with", "sharing", "mutable", "cyclic"
           , "datatype", "constructor"
           , "graph", "block"
           , "for", "from"
           , "end"
           , "and", "or", "not", "is", "raises"
           ]

openSpace :: Bool -> Char -> Parse ()
openSpace s c = tok $ \case Open [c] s -> Just ()
                            _          -> Nothing

parenWithSpace = openSpace True  '('
parenNoSpace   = openSpace False '('

angleWithSpace = openSpace True  '<'
angleNoSpace   = openSpace False '<'

open :: Char -> Parse ()
open c = tok $ \case Open [c] _ -> Just ()
                     _          -> Nothing

close :: Char -> Parse ()
close c = tok $ \case Close [c] -> Just ()
                      _         -> Nothing
