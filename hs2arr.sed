s~\[\([^]]*\)\]~List<\1>~g
s~Double~Number~g
s~^[ ]*deriving.*~end~
s~^module.*~#lang pyret~

s~^data \(.*\)~data \1:~
s~^data \([^ ]*\) \(.*\)~data \1<\2~
#s~^data\(.*<\)[ \([^ ]*\)]*~data\1, \2~

s~^  =~  |~
s~  | \([^ ]*\) \(.*\)~  | \1 (\2 )~
