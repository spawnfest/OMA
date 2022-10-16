%% From documentation.

Definitions.

D = [0-9]

Rules.

{D}+ :
  {token,{integer,TokenLoc,list_to_integer(TokenChars)}}.

{D}+\.{D}+((E|e)(\+|\-)?{D}+)? :
  {token,{float,TokenLoc,list_to_float(TokenChars)}}.

Erlang code.