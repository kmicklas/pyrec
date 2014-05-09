{
module Pyrec.Lex (Tok(..), Token(..), scan) where

import Text.Parsec.Pos

import Pyrec.Misc
}

%wrapper "posn"

$digit     = [0-9]
$idenStart = [a-zA-Z_]
$idenChar  = [a-zA-Z_0-9\-]

tokens :-

  $white+                             ;
  "#".*                               ;
  "+"|"-"|"*"|"/"|
  "<="|">="|"=="|
  "<>"|">"|"<"|
  \:|\,|\=|\:\:|\-\>|\.|
  and|or|not|is|raises
  import|provide|as|
  var|
  fun|method|doc:|
  where:|
  check:|
  try:|except|
  cases:|
  when|if:|then:|if|else\ if|else:|
  data|with:|sharing:|mutable|cyclic|
  datatype|with\ constructor|
  graph:|block:|
  for|from|
  end|\;                              { tok $ \ s -> Kw s }
  $digit+                             { tok $ \ s -> TNum (read s) }
  $idenStart$idenChar*                { tok $ \ s -> Iden s }
  [\(\<\[\{]                          { tok $ \ s -> Open  s False }
  [\)\>\]\}]                          { tok $ \ s -> Close s       }
  .                                   { tok $ \ s -> Error s       }
  \"(\n|[^\"\n])*\"                   { tok $ \ s -> TStr (read s) }

{
data Token = Token SourcePos Tok

instance Show Token where
  show (Token _ t) = showTok t

tok :: (String -> Tok) -> AlexPosn -> String -> (AlexPosn, Tok)
tok f p s = (p, f s)

data Tok
  = Kw    String
  | Iden  String
  | TNum  Double
  | TStr  String
  | Open  String Bool
  | Close String
  | Error String -- for lexical errors
  deriving (Eq, Show, Ord)

showTok (Kw w)     = w
showTok (Iden w)   = w
showTok (TNum n)   = show n
showTok (TStr s)   = show s
showTok (Open s _) = s
showTok (Close s)  = s
showTok (Error s)  = s

scan :: String -> String -> [Token]
scan file source =
  for (alexScanTokens source) $ \ (AlexPn _ l c, t) ->
    Token (newPos file l c) t
}
