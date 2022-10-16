%% Comma-separated integers.

Definitions.

D = [0-9]
W = [\s\n]

Rules.

{W}+ :
  skip_token.

{D}+ :
  {token,{integer,{TokenLine, TokenCol},list_to_integer(TokenChars)}}.

Erlang code.