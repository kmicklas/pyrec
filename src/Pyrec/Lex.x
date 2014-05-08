{
module Pyrec.Lex (Tok(..), Token(..), scan) where

import Text.Parsec.Pos

import Pyrec.Misc
}

%wrapper "posn"

$digit     = [0-9]
$idenStart = [a-zA-Z]
$idenChar  = [a-zA-Z0-9\-]

tokens :-

  $white+                             ;
  "#".*                               ;
  $digit+                             { tok $ \ s -> TNum (read s) }
  $idenStart$idenChar*                { tok $ \ s -> Iden s }
  "+"|"-"|"*"|"/"|
  "<="|">="|"=="|
  "<>"|">"|"<"|
  \:|\,|
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
  for|from                            { tok $ \ s -> Kw s }
  [\(\<\[\{]                          { tok $ \ s -> Open  s False }
  [\)\>\]\}]                         { tok $ \ s -> Close s       }
  .                                   { tok $ \ s -> Error s       }

{
data Token = Token SourcePos Tok

instance Show Token where
  show (Token _ t) = show t

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

scan :: String -> String -> [Token]
scan file source =
  for (alexScanTokens source) $ \ (AlexPn _ l c, t) ->
    Token (newPos file l c) t
}
