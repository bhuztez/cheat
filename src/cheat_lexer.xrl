Definitions.

Atom       = [a-z][0-9a-zA-Z_]*
Variable   = [A-Z][0-9a-zA-Z_]*
Ignore     = _[0-9a-zA-Z_]*
Integer    = [0-9]+
Keyword    = ([={}\[\]|(),;:\.+\-*/%<>]|<-|\|\||->|\*\*|==|/=|=<|>=|=:=|=/=|<<|>>|after|and|andalso|band|begin|bnot|bor|bsl|bsr|bxor|case|catch|cond|div|end|fun|if|let|not|of|or|orelse|receive|rem|try|when|xor)
Whitespace = [\000-\s]+
Comment    = %[^\n]+
String     = "([^"]|\\")+"

Rules.

{Keyword}    : {token, {list_to_atom(TokenChars), TokenLine}}.
{Integer}    : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{Atom}       : {token, {atom, TokenLine, list_to_atom(TokenChars)}}.
{Variable}   : {token, {varname, TokenLine, list_to_atom(TokenChars)}}.
{Ignore}     : {token, {ignore, TokenLine, list_to_atom(TokenChars)}}.
{String}     : {token, {string, TokenLine, TokenChars}}.
{Whitespace} : skip_token.
{Comment}    : skip_token.

Erlang code.
