%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2010-2022. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%
-module(leex_column_SUITE).

%-define(debug, true).

-include_lib("kernel/include/file.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([
	 man/1, ex/1, ex2/1, unicode/1,
	 line_wrap/1,
	 otp_10302/1, otp_13916/1]).

-include_lib("tutil.hrl").

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, test_server:minutes(1)).

init_per_testcase(_Case, Config) ->
    Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group, examples}, {group, tickets}, {group, bugs}].

groups() -> 
    [{examples, [], [man, ex, ex2, unicode]},
    {tickets, [], [otp_10302, otp_13916]},
    {bugs, [], [line_wrap]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

unicode(suite) ->
    [];
unicode(Config) when is_list(Config) ->
    Ts = [{unicode_1, 
	   <<"%% -*- coding: utf-8 -*-\n"
	     "Definitions.\n "
	     "RTLarrow    = (â)\n"
	     "Rules.\n"
	     "{RTLarrow}  : {token,{\"â\",TokenLoc}}.\n"
	     "Erlang code.\n"
	     "-export([t/0]).\n"
	     "t() -> {ok, [{\"â\", {1,1}}], {1,4}} = string(\"â\"), ok.">>,
           default,
           [{error_location, column}],
           ok}],

    run(Config, Ts),
    ok.

man(doc) ->
    "Examples from the manpage.";
man(suite) -> [];
man(Config) when is_list(Config) ->
    Ts = [{man_1, 
     <<"Definitions.\n"
       "Rules.\n"
       "[a-z][0-9a-zA-Z_]* :\n"
       "    {token,{atom,TokenLoc,list_to_atom(TokenChars)}}.\n"
       "[A-Z_][0-9a-zA-Z_]* :\n"
       "    {token,{var,TokenLoc,list_to_atom(TokenChars)}}.\n"
       "(\\+|-)?[0-9]+\\.[0-9]+((E|e)(\\+|-)?[0-9]+)? : \n"
       "   {token,{float,TokenLoc,list_to_float(TokenChars)}}.\n"
       "\\s : skip_token.\n"
       "Erlang code.\n"
       "-export([t/0]).\n"
       "t() ->\n"
       "    {ok,[{float,{1,1},3.14},{atom,{1,5},atom},{var,{1,10},'V314'}],{1,14}} =\n"
       "        string(\"3.14atom V314\"),\n"
       "    ok.\n">>,
           default,
           [{error_location, column}],
           ok},

          {man_2,
     <<"Definitions.\n"
       "D = [0-9]\n"
       "Rules.\n"
       "{D}+ :\n"
       "  {token,{integer,TokenLoc,list_to_integer(TokenChars)}}.\n"
       "{D}+\\.{D}+((E|e)(\\+|\\-)?{D}+)? :\n"
       "  {token,{float,TokenLoc,list_to_float(TokenChars)}}.\n"
       "\\s : skip_token.\n"
       "Erlang code.\n"
       "-export([t/0]).\n"
       "t() ->\n"
       "    {ok,[{float,{1,1},3.14},{integer,{1,6},314}],{1,9}} = \n"
       "        string(\"3.14 314\"),\n"
       "    ok.\n">>,
           default,
           [{error_location, column}],
           ok}],
    
    run(Config, Ts),
    ok.

ex(doc) ->
    "Examples.";
ex(suite) -> [];
ex(Config) when is_list(Config) ->
    Ts = [{ex_1,
      <<"Definitions.\n"
        "D = [0-543-705-982]\n"
        "Rules.\n"
        "{D}+ :\n"
        "  {token,{integer,TokenLoc,list_to_integer(TokenChars)}}.\n"
        "[^235]+ :\n"
        "  {token,{list_to_atom(TokenChars),TokenLoc}}.\n"
        "Erlang code.\n"
        "-export([t/0]).\n"
        "t() ->\n"
        "    {ok,[{integer,{1,1},12},{' c\\na',{1,3}},{integer,{2,2},34},{b789a,{2,4}}],{2,9}} =\n"
        "        string(\"12 c\\na34b789a\"),\n"
        "    ok.\n">>,
           default,
           [{error_location, column}],
           ok}
        ],
    run(Config, Ts),
    ok.

ex2(doc) ->
    "More examples.";
ex2(suite) -> [];
ex2(Config) when is_list(Config) ->
    Xrl = 
     <<"
%%% File : erlang_scan.xrl
%%% Author : Robert Virding
%%% Purpose : Token definitions for Erlang.
 
Definitions.
O  = [0-7]
D  = [0-9]
H  = [0-9a-fA-F]
U  = [A-Z]
L  = [a-z]
A  = ({U}|{L}|{D}|_|@)
WS  = ([\\000-\\s]|%.*)
 
Rules.
{D}+\\.{D}+((E|e)(\\+|\\-)?{D}+)? :
      {token,{float,TokenLoc,list_to_float(TokenChars)}}.
{D}+#{H}+  :  base(TokenLine, TokenChars).
{D}+    :  {token,{integer,TokenLoc,list_to_integer(TokenChars)}}.
{L}{A}*    :  Atom = list_to_atom(TokenChars),
      {token,case reserved_word(Atom) of
         true -> {Atom,TokenLine};
         false -> {atom,TokenLine,Atom}
       end}.
'(\\\\\\^.|\\\\.|[^'])*' :
      %% Strip quotes.
      S = lists:sublist(TokenChars, 2, TokenLen - 2),
      case catch list_to_atom(string_gen(S)) of
       {'EXIT',_} -> {error,\"illegal atom \" ++ TokenChars};
       Atom -> {token,{atom,TokenLine,Atom}}
      end.
({U}|_){A}*  :  {token,{var,TokenLoc,list_to_atom(TokenChars)}}.
\"(\\\\\\^.|\\\\.|[^\"])*\" :
      %% Strip quotes.
      S = lists:sublist(TokenChars, 2, TokenLen - 2),
      {token,{string,TokenLoc,string_gen(S)}}.
\\$(\\\\{O}{O}{O}|\\\\\\^.|\\\\.|.) :
      {token,{char,TokenLoc,cc_convert(TokenChars)}}.
->    :  {token,{'->',TokenLine}}.
:-    :  {token,{':-',TokenLine}}.
\\|\\|    :  {token,{'||',TokenLine}}.
<-    :  {token,{'<-',TokenLine}}.
\\+\\+    :  {token,{'++',TokenLine}}.
--    :  {token,{'--',TokenLine}}.
=/=    :  {token,{'=/=',TokenLine}}.
==    :  {token,{'==',TokenLine}}.
=:=    :  {token,{'=:=',TokenLine}}.
/=    :  {token,{'/=',TokenLine}}.
>=    :  {token,{'>=',TokenLine}}.
=<    :  {token,{'=<',TokenLine}}.
<=    :  {token,{'<=',TokenLine}}.
<<    :  {token,{'<<',TokenLine}}.
>>    :  {token,{'>>',TokenLine}}.
::    :  {token,{'::',TokenLine}}.
[]()[}{|!?/;:,.*+#<>=-] :
      {token,{list_to_atom(TokenChars),TokenLine}}.
\\.{WS}    :  {end_token,{dot,TokenLine}}.
{WS}+    :  skip_token.
 
Erlang code.
 
-export([reserved_word/1]).

%% reserved_word(Atom) -> Bool
%% return 'true' if Atom is an Erlang reserved word, else 'false'.

reserved_word('after') -> true;
reserved_word('begin') -> true;
reserved_word('case') -> true;
reserved_word('try') -> true;
reserved_word('cond') -> true;
reserved_word('catch') -> true;
reserved_word('andalso') -> true;
reserved_word('orelse') -> true;
reserved_word('end') -> true;
reserved_word('fun') -> true;
reserved_word('if') -> true;
reserved_word('let') -> true;
reserved_word('of') -> true;
reserved_word('receive') -> true;
reserved_word('when') -> true;
reserved_word('bnot') -> true;
reserved_word('not') -> true;
reserved_word('div') -> true;
reserved_word('rem') -> true;
reserved_word('band') -> true;
reserved_word('and') -> true;
reserved_word('bor') -> true;
reserved_word('bxor') -> true;
reserved_word('bsl') -> true;
reserved_word('bsr') -> true;
reserved_word('or') -> true;
reserved_word('xor') -> true;
reserved_word('spec') -> true;
reserved_word(_) -> false.

base(L, Cs) ->
    H = string:chr(Cs, $#),
    case list_to_integer(string:substr(Cs, 1, H-1)) of
        B when B > 16 -> {error,\"illegal base\"};
        B ->
            case base(string:substr(Cs, H+1), B, 0) of
                error -> {error,\"illegal based number\"};
                N -> {token,{integer,L,N}}
            end
    end.

base([C|Cs], Base, SoFar) when C >= $0, C =< $9, C < Base + $0 ->
    Next = SoFar * Base + (C - $0),
    base(Cs, Base, Next);
base([C|Cs], Base, SoFar) when C >= $a, C =< $f, C < Base + $a - 10 ->
    Next = SoFar * Base + (C - $a + 10),
    base(Cs, Base, Next);
base([C|Cs], Base, SoFar) when C >= $A, C =< $F, C < Base + $A - 10 ->
    Next = SoFar * Base + (C - $A + 10),
    base(Cs, Base, Next);
base([_|_], _, _) -> error;      %Unknown character
base([], _, N) -> N.

cc_convert([$$,$\\\\|Cs]) ->
    hd(string_escape(Cs));
cc_convert([$$,C]) -> C.

string_gen([$\\\\|Cs]) ->
    string_escape(Cs);
string_gen([C|Cs]) ->
    [C|string_gen(Cs)];
string_gen([]) -> [].

string_escape([O1,O2,O3|S]) when
        O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    [(O1*8 + O2)*8 + O3 - 73*$0|string_gen(S)];
string_escape([$^,C|Cs]) ->
    [C band 31|string_gen(Cs)];
string_escape([C|Cs]) when C >= $\\000, C =< $\\s ->
    string_gen(Cs);
string_escape([C|Cs]) ->
    [escape_char(C)|string_gen(Cs)].

escape_char($n) -> $\\n;        %\\n = LF
escape_char($r) -> $\\r;        %\\r = CR
escape_char($t) -> $\\t;        %\\t = TAB
escape_char($v) -> $\\v;        %\\v = VT
escape_char($b) -> $\\b;        %\\b = BS
escape_char($f) -> $\\f;        %\\f = FF
escape_char($e) -> $\\e;        %\\e = ESC
escape_char($s) -> $\\s;        %\\s = SPC
escape_char($d) -> $\\d;        %\\d = DEL
escape_char(C) -> C.
      ">>,
    Dir = ?privdir,
    XrlFile = filename:join(Dir, "erlang_scan.xrl"),
    ok = file:write_file(XrlFile, Xrl),
    ErlFile = filename:join(Dir, "erlang_scan.erl"),
    {ok, _} = leex:file(XrlFile, [{error_location, column}]),
    {ok, _} = compile:file(ErlFile, [{outdir,Dir}]),
    code:purge(erlang_scan),
    AbsFile = filename:rootname(ErlFile, ".erl"),
    code:load_abs(AbsFile, erlang_scan),

    F = fun(Cont, Chars, Line, Col) ->
                erlang_scan:tokens(Cont, Chars, Line, Col)
        end,
    F1 = fun(Cont, Chars, Line, Col) ->
                 erlang_scan:token(Cont, Chars, Line, Col)
         end,
    fun() ->
            S = "ab cd. ",
            {ok, Ts, {1,8}} = scan_tokens_1(S, F, 1),
            {ok, Ts, {1,8}} = scan_token_1(S, F1, 1),
            {ok, Ts, {1,8}} = scan_tokens(S, F, 1),
            {ok, Ts, {1,8}} = erlang_scan:string(S, 1)
    end(),
    fun() ->
            S = "'ab\n cd'. ",
            {ok, Ts, {2,7}} = scan_tokens_1(S, F, 1),
            {ok, Ts, {2,7}} = scan_token_1(S, F1, 1),
            {ok, Ts, {2,7}} = scan_tokens(S, F, 1),
            {ok, Ts, {2,7}} = erlang_scan:string(S, 1)
    end(),
    fun() ->
            S = "99. ",
            {ok, Ts, {1,5}} = scan_tokens_1(S, F, 1),
            {ok, Ts, {1,5}} = scan_token_1(S, F1, 1),
            {ok, Ts, {1,5}} = scan_tokens(S, F, 1),
            {ok, Ts, {1,5}} = erlang_scan:string(S, 1)
    end(),
    {ok,[{integer,{1,1},99},{dot,1}],{1,5}} = erlang_scan:string("99. "),
    fun() ->
            Atom = "'" ++ lists:duplicate(1000,$a) ++ "'",
            S = Atom ++ ". ",
            Reason = "illegal atom " ++ Atom,
            Err = {error,{{1,1003},erlang_scan,{user,Reason}},{1,1003}},
            {done,Err,[]} = scan_tokens_1(S, F, 1),
            {done,Err,[]} = scan_token_1(S, F1, 1),
            {done,Err,[]} = scan_tokens(S, F, 1),
            Err = erlang_scan:string(S, 1)
    end(),
    fun() ->
            S = "\x{aaa}. ",
            Err = {error,{{1,1},erlang_scan,{illegal,[2730]}},{1,1}},
            {done,Err,[]} = scan_tokens_1(S, F, 1),
            {done,Err,[_]} = scan_token_1(S, F1, 1), % Note: Rest non-empty
            {done,Err,[]} = scan_tokens(S, F, 1),
            Err = erlang_scan:string(S, 1)
    end(),
    fun() ->
            S = "\x{aaa} + 1. 34",
            Err = {error,{{1,1},erlang_scan,{illegal,[2730]}},{1,1}},
            {done,Err,[]} = scan_tokens_1(S, F, 1),
            {done,Err,[_]} = scan_token_1(S, F1, 1), % Note: Rest non-empty
            {done,Err,"34"} = scan_tokens(S, F, 1),
            Err = erlang_scan:string(S, 1)
    end(),
    fun() ->
            S = "\x{aaa} \x{bbb}. 34",
            Err = {error,{{1,1},erlang_scan,{illegal,[2730]}},{1,1}},
            {done,Err,[]} = scan_tokens_1(S, F, 1),
            {done,Err,[_]} = scan_token_1(S, F1, 1), % Note: Rest non-empty
            {done,Err,"34"} = scan_tokens(S, F, 1),
            Err = erlang_scan:string(S, 1)
    end(),
    fun() ->
            S = "\x{aaa} 18#34. 34",
            Err = {error,{{1,1},erlang_scan,{illegal,[2730]}},{1,1}},
            {done,Err,[]} = scan_tokens_1(S, F, 1),
            {done,Err,[_]} = scan_token_1(S, F1, 1), % Note: Rest non-empty
            {done,Err,"34"} = scan_tokens(S, F, 1),
            Err = erlang_scan:string(S, 1)
    end(),
    fun() ->
            S = "\x{aaa}"++eof,
            Err = {error,{{1,1},erlang_scan,{illegal,[2730]}},{1,1}},
            {done,Err,eof} = scan_tokens_1(S, F, 1),
            {done,Err,[_]} = scan_token_1(S, F1, 1), % Note: Rest non-empty
            {done,Err,eof} = scan_tokens(S, F, 1),
            Err = erlang_scan:string(S, 1)
    end(),
    ok.

scan_tokens(String, Fun, Line) ->
    scan_tokens(String, Fun, Line, 1, []).

scan_tokens(String, Fun, Line, Col, Rs) ->
    case Fun([], String, Line, Col) of
        {done, {error,_,_}, _} = Error ->
            Error;
        {done, {ok,Ts,End}, ""} ->
            {ok, lists:append(lists:reverse([Ts|Rs])), End};
        {done, {ok,Ts,{Line, Col}}, Rest} ->
            scan_tokens(Rest, Fun, Line, Col, [Ts|Rs])
    end.

scan_tokens_1(String, Fun, Line) ->
    scan_tokens_1({more, []}, String, Fun, Line, 1, []).

scan_tokens_1({done, {error, _, _}, _}=Error, _Cs, _Fun, _Line, _Col, _Rs) ->
    Error;
scan_tokens_1({done, {ok,Ts,End}, ""}, "", _Fun, _Line, _Col, Rs) ->
    {ok,lists:append(lists:reverse([Ts|Rs])),End};
scan_tokens_1({done, {ok,Ts,{Line, Col}}, Rest}, Cs, Fun, _Line, _Col, Rs) ->
    scan_tokens_1({more,[]}, Rest++Cs, Fun, Line, Col, [Ts|Rs]);
scan_tokens_1({more, Cont}, [C | Cs], Fun, Line, Col, Rs) ->
    R = Fun(Cont, [C], Line, Col),
    scan_tokens_1(R, Cs, Fun, Line, Col, Rs);
scan_tokens_1({more, Cont}, eof, Fun, Line, Col, Rs) ->
    R = Fun(Cont, eof, Line, Col),
    scan_tokens_1(R, eof, Fun, Line, Col, Rs).

scan_token_1(String, Fun, Line) ->
    scan_token_1({more, []}, String, Fun, Line, 1, []).

scan_token_1({done, {error, _, _}, _}=Error, _Cs, _Fun, _Line, _Col, _Rs) ->
    Error;
scan_token_1({done, {ok,Ts,End}, ""}, "", _Fun, _Line, _Col, Rs) ->
    {ok,lists:reverse([Ts|Rs]),End};
scan_token_1({done, {ok,Ts,{Line, Col}}, Rest}, Cs, Fun, _Line, _Col, Rs) ->
    scan_token_1({more,[]}, Rest++Cs, Fun, Line, Col, [Ts|Rs]);
scan_token_1({more, Cont}, [C | Cs], Fun, Line, Col, Rs) ->
    R = Fun(Cont, [C], Line,Col),
    scan_token_1(R, Cs, Fun, Line, Col, Rs).

%% End of ex2

line_wrap(doc) ->    "Much more examples.";
line_wrap(suite) -> [];
line_wrap(Config) when is_list(Config) ->
    Xrl =
     <<"
Definitions.
Rules.
[a]+[\\n]*= : {token, {first, TokenLine}}.
[a]+ : {token, {second, TokenLine}}.
[\\s\\r\\n\\t]+ : skip_token.
Erlang code.
      ">>,
    Dir = ?privdir,
    XrlFile = filename:join(Dir, "test_line_wrap.xrl"),
    ok = file:write_file(XrlFile, Xrl),
    ErlFile = filename:join(Dir, "test_line_wrap.erl"),
    {ok, _} = leex:file(XrlFile, [{error_location, column}]),
    {ok, _} = compile:file(ErlFile, [{outdir,Dir}]),
    code:purge(test_line_wrap),
    AbsFile = filename:rootname(ErlFile, ".erl"),
    code:load_abs(AbsFile, test_line_wrap),
    fun() ->
            S = "aaa\naaa",
            {ok,[{second,1},{second,2}],{2,4}} = test_line_wrap:string(S)
    end(),
    fun() ->
            S = "aaa\naaa",
            {ok,[{second,3},{second,4}],{4,4}} = test_line_wrap:string(S, 3)
    end(),
    fun() ->
            {done,{ok,{second,1},{1,2}},"\na"} = test_line_wrap:token([], "a\na"),
            {more,{token,_,2,1,_,_,_,_,_,_} = Cont1} = test_line_wrap:token([], "\na"),
            {done,{ok,{second,2},{2,2}},eof} = test_line_wrap:token(Cont1, eof)
    end(),
    fun() ->
            {more,Cont1} = test_line_wrap:tokens([], "a\na"),
            {done,{ok,[{second,1},{second,2}],{2,2}},eof} = test_line_wrap:tokens(Cont1, eof)
    end(),
    ok.

%% End of line_wrap

otp_10302(doc) ->
    "OTP-10302. Unicode characters scanner/parser.";
otp_10302(suite) -> [];
otp_10302(Config) when is_list(Config) ->
    Dir = ?privdir,
    Filename = filename:join(Dir, "file.xrl"),
    Ret = [return, {report, true}],

    ok = file:write_file(Filename,<<
         "%% coding: UTF-8\n"
         "ä"
     >>),
    {error,[{_,[{2,leex,cannot_parse}]}],[]} =
        leex:file(Filename, Ret),

    ok = file:write_file(Filename,<<
         "%% coding: UTF-8\n"
         "Definitions.\n"
         "ä"
     >>),
    {error,[{_,[{3,leex,cannot_parse}]}],[]} = leex:file(Filename, Ret),

    ok = file:write_file(Filename,<<
         "%% coding: UTF-8\n"
         "Definitions.\n"
         "A = a\n"
         "L = [{A}-{Z}]\n"
         "Z = z\n"
         "Rules.\n"
         "{L}+ : {token,{list_to_atom(TokenChars),Häpp}}.\n"
     >>),
    {error,[{_,[{7,leex,cannot_parse}]}],[]} = leex:file(Filename, Ret),

    ok = file:write_file(Filename,<<
         "%% coding: UTF-8\n"
         "Definitions.\n"
         "A = a\n"
         "L = [{A}-{Z}]\n"
         "Z = z\n"
         "Rules.\n"
         "{L}+ : {token,{list_to_atom(TokenChars)}}.\n"
         "Erlang code.\n"
         "-export([t/0]).\n"
         "t() ->\n"
         "    Häpp\n"
      >>),
    {error,[{_,[{11,leex,cannot_parse}]}],[]} = leex:file(Filename, Ret),

    Mini = <<"Definitions.\n"
             "D  = [0-9]\n"
             "Rules.\n"
             "{L}+  : {token,{word,TokenLoc,TokenChars}}.\n"
             "Erlang code.\n">>,
    LeexPre = filename:join(Dir, "leexinc.hrl"),
    ok = file:write_file(LeexPre, <<"%% coding: UTF-8\n ä">>),
    PreErrors = run_test(Config, Mini, LeexPre),
    {error,[{IncludeFile,[{2,leex,cannot_parse}]}],[]} = PreErrors,
    "leexinc.hrl" = filename:basename(IncludeFile),

    Ts = [{uni_1,
       <<"%% coding: UTF-8\n"
         "Definitions.\n"
         "A = a\n"
         "L = [{A}-{Z}]\n"
         "Z = z\n"
         "Rules.\n"
         "{L}+ : {token,{list_to_atom(TokenChars),\n"
         "begin HÃ¤pp = foo, HÃ¤pp end,"
         " 'HÃ¤pp',\"\\x{400}B\",\"Ã¶rn_Ð\"}}.\n"
         "Erlang code.\n"
         "-export([t/0]).\n"
         "t() ->\n"
         "    %% HÃ¤pp, 'HÃ¤pp',\"\\x{400}B\",\"Ã¶rn_Ð\"\n"
         "    {ok, [R], {1,4}} = string(\"tip\"),\n"
         "    {tip,foo,'HÃ¤pp',[1024,66],[246,114,110,95,1024]} = R,\n"
         "    HÃ¤pp = foo,\n"
         "    {tip, HÃ¤pp, 'HÃ¤pp',\"\\x{400}B\",\"Ã¶rn_Ð\"} = R,\n"
         "    ok.\n">>,
          default,
          [{error_location, column}],
          ok},
      {uni_2,
       <<"%% coding: Latin-1\n"
         "Definitions.\n"
         "A = a\n"
         "L = [{A}-{Z}]\n"
         "Z = z\n"
         "Rules.\n"
         "{L}+ : {token,{list_to_atom(TokenChars),\n"
         "begin Häpp = foo, Häpp end,"
         " 'Häpp',\"\\x{400}B\",\"Ã¶rn_Ð\"}}.\n"
         "Erlang code.\n"
         "-export([t/0]).\n"
         "t() ->\n"
         "    %% Häpp, 'Häpp',\"\\x{400}B\",\"Ã¶rn_Ð\"\n"
         "    {ok, [R], {1,4}} = string(\"tip\"),\n"
         "    {tip,foo,'Häpp',[1024,66],[195,182,114,110,95,208,128]} = R,\n"
         "    Häpp = foo,\n"
         "    {tip, Häpp, 'Häpp',\"\\x{400}B\",\"Ã¶rn_Ð\"} = R,\n"
         "    ok.\n">>,
          default,
          [{error_location, column}],
          ok}],
    run(Config, Ts),
    ok.

otp_13916(doc) ->
    "OTP-13916. Leex rules with newlines result in bad line numbers";
otp_13916(suite) -> [];
otp_13916(Config) when is_list(Config) ->
    Ts = [{otp_13916_1,
           <<"Definitions.\n"
             "W = [a-zA-Z0-9]\n"
             "S = [\\s\\t]\n"
             "B = [\\n\\r]\n"
             "Rules.\n"
             "%% mark line break(s) and empty lines by token 'break'\n"
             "%% in order to use as delimiters\n"
             "{B}({S}*{B})+ : {token, {break,   TokenLoc}}.\n"
             "{B}           : {token, {break,   TokenLoc}}.\n"
             "{S}+          : {token, {blank,   TokenLoc, TokenChars}}.\n"
             "{W}+          : {token, {word,    TokenLoc, TokenChars}}.\n"
             "Erlang code.\n"
             "-export([t/0]).\n"
             "t() ->\n"
             "    {ok,[{break,{1,1}},{blank,{4,1},\"  \"},{word,{4,3},\"breaks\"}],{4,9}} =\n"
             "        string(\"\\n\\n  \\n  breaks\"),\n"
             "{ok,[{break,{1,1}},{word,{4,1},\"works\"}],{4,6}} =\n"
             "        string(\"\\n\\n  \\nworks\"),\n"
             "    {ok,[{break,{1,1}},{word,{4,1},\"L4\"},{break,{4,3}},\n"
             "         {word,{5,1},\"L5\"},{break,{5,3}},{word,{7,1},\"L7\"}], {7,3}} =\n"
             "        string(\"\\n\\n  \\nL4\\nL5\\n\\nL7\"),\n"
             "{ok,[{break,{1,1}},{blank,{4,1},\" \"},{word,{4,2} ,\"L4\"},\n"
             "     {break,{4,4}},{blank,{5,1},\" \"},{word,{5,2},\"L5\"},\n"
             "     {break,{5,4}},{blank,{7,1},\" \"},{word,{7,2},\"L7\"}], {7,4}} =\n"
             "        string(\"\\n\\n  \\n L4\\n L5\\n\\n L7\"),\n"
             
             "    ok.\n">>,
           default,
           [{error_location, column}],
           ok}],
    run(Config, Ts),
    ok.