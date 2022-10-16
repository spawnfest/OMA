%% Comma-separated integers.

Definitions.

D = [0-9]
W = [\s\n]

Rules.

{W}+ :
  skip_token.

{D}+ :
  {token,{integer,TokenLoc,list_to_integer(TokenChars)}}.

Erlang code.

-export([t/0]).
t() ->
  {ok,[{float, {1,1}, 4.44}],{1,5}} = string("4.44"),
  ok.