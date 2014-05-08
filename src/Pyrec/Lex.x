{
module Pyrec.Lex (Tok, scan) where

import Text.Parsec.Pos

import Pyrec.Misc
}

%wrapper "posn"

$digit     = [0-9]
$idenStart = [a-zA-Z]
$idenChar  = [a-zA-Z0-9\-]

tokens :-

  $white+               ;
  "#".*                 ;
  $digit+               { tok $ \s -> Num (read s) }
  $idenStart$idenChar*  { tok $ \s -> Iden s }

{
type Token = (AlexPosn, Tok)

tok :: (String -> Tok) -> AlexPosn -> String -> Token
tok f p s = (p, f s)

data Tok
  = Kw String
  | Iden String
  | Num Double
  deriving (Eq, Show, Ord)

scan :: String -> String -> [(SourcePos, Tok)]
scan file source =
  for (alexScanTokens source) $ \ (AlexPn _ l c, t) -> (newPos file l c, t)
}
