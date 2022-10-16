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
-module(column_SUITE).

%-define(debug, true).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([
	 number/1, end_loc/1, tab/1, badargs/1, token_loc_var/1]).

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
    [number, end_loc, tab, badargs, token_loc_var].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Additional tests %%%
%%%%%%%%%%%%%%%%%%%%%%%%

number(Config) ->
    Ts = [{number,
      <<"Definitions.\n"
        "D = [0-9]\n"
        "Rules.\n"
        "{D}+ :\n"
        "{token,{integer,{TokenLine,TokenCol},list_to_integer(TokenChars)}}.\n"
        "{D}+\\.{D}+((E|e)(\\+|\\-)?{D}+)? :\n"
        "{token,{float,{TokenLine,TokenCol},list_to_float(TokenChars)}}.\n"
        "Erlang code.\n"
        "-export([t/0]).\n"
        "t() ->\n"
        "{ok,[{float, {1,1}, 4.44}],{1,5}} = string(\"4.44\"), ok.\n">>,
           default,
           [{error_location,column}],
           ok},
        {number_multiline,
      <<"Definitions.\n"
        "D = [0-9]\n"
        "W = [\\s\\n]\n"
        "Rules.\n"
        "{W}+ :\n"
        "skip_token.\n"
        "{D}+ :\n"
        "{token,{integer,{TokenLine,TokenCol},list_to_integer(TokenChars)}}.\n"
        "{D}+\\.{D}+((E|e)(\\+|\\-)?{D}+)? :\n"
        "{token,{float,{TokenLine,TokenCol},list_to_float(TokenChars)}}.\n"
        "Erlang code.\n"
        "-export([t/0]).\n"
        "t() ->\n"
        "{ok,[{float, {2,1}, 4.44},{integer, {3,3}, 5},{integer, {7,3}, 7}],{8,2}} = string(\"\n4.44  \n  5 \n  \n\n\n  7 \n \"), ok.\n">>,
           default,
           [{error_location,column}],
           ok}],
    run(Config, Ts),
    ok.

end_loc(Config) ->
    Ts = [
        {nl_last,
        <<"Definitions.\n"
            "Rules.\n"
            "[a]+[\\n]*= : {token, {first, {TokenLine,TokenCol}}}.\n"
            "[a]+ : {token, {second, {TokenLine,TokenCol}}}.\n"
            "[\\s\\r\\n\\t]+ : skip_token.\n"
            "Erlang code.\n"
            "-export([t/0]).\n"
            "t() ->\n"
            "{ok,[{second,{1,1}}],{2,1}} = string(\"a\\n\"), ok.\n">>,
      default,
      [{error_location,column}],
      ok}],
    run(Config, Ts),
    ok.

tab(Config) ->
    Ts = [
        {tab1,
        <<"Definitions.\n"
            "Rules.\n"
            "[a]+[\\n]*= : {token, {first, {TokenLine,TokenCol}}}.\n"
            "[a]+ : {token, {second, {TokenLine,TokenCol}}}.\n"
            "[\\s\\r\\n\\t]+ : skip_token.\n"
            "Erlang code.\n"
            "-export([t/0]).\n"
            "t() ->\n"
            "{ok,[{second,{1,9}}],{2,1}} = string(\"\ta\\n\"), ok.\n">>,
      default,
      [{error_location,column}],
      ok},
    {tab2,
      <<"Definitions.\n"
          "Rules.\n"
          "[a]+[\\n]*= : {token, {first, {TokenLine,TokenCol}}}.\n"
          "[a]+ : {token, {second, {TokenLine,TokenCol}}}.\n"
          "[\\s\\r\\n\\t]+ : skip_token.\n"
          "Erlang code.\n"
          "-export([t/0]).\n"
          "t() ->\n"
          "{ok,[{second,{1,9}}],{2,1}} = string(\"  \ta\\n\"), ok.\n">>,
        default,
        [{error_location,column}],
        ok},
    {tab3,
        <<"Definitions.\n"
            "Rules.\n"
            "[a]+[\\n]*= : {token, {first, {TokenLine,TokenCol}}}.\n"
            "[a]+ : {token, {second, {TokenLine,TokenCol}}}.\n"
            "[\\s\\r\\n\\t]+ : skip_token.\n"
            "Erlang code.\n"
            "-export([t/0]).\n"
            "t() ->\n"
            "{ok,[{second,{1,9}}],{2,1}} = string(\"      \ta\\n\"), ok.\n">>,
        default,
        [{error_location,column}],
        ok},
    {tab4,
        <<"Definitions.\n"
            "Rules.\n"
            "[a]+[\\n]*= : {token, {first, {TokenLine,TokenCol}}}.\n"
            "[a]+ : {token, {second, {TokenLine,TokenCol}}}.\n"
            "[\\s\\r\\n\\t]+ : skip_token.\n"
            "Erlang code.\n"
            "-export([t/0]).\n"
            "t() ->\n"
            "{ok,[{second,{1,27}},{second,{2,19}}],{2,25}} = string(\"   \t \t\t  a\\n \t \t  aaa\t\"), ok.\n">>,
        default,
        [{error_location,column}],
        ok},
    {tab5,
        <<"Definitions.\n"
            "Rules.\n"
            "[a]+[\\n]*= : {token, {first, {TokenLine,TokenCol}}}.\n"
            "[a]+ : {token, {second, {TokenLine,TokenCol}}}.\n"
            "[\\s\\r\\n\\t]+ : skip_token.\n"
            "Erlang code.\n"
            "-export([t/0]).\n"
            "t() ->\n"
            "{ok,[{second,{1,15}},{second,{2,9}}],{2,16}} = string(\"   \t \t\t  a\\n \t \t  aaa\t\"), ok.\n">>,
        default,
        [{tab_size,3},{error_location,column}],
        ok}],
    run(Config, Ts),
    ok.

badargs(Config) ->
    X1 =  <<"Definitions.\n"
            "Rules.\n"
            "[a]+[\\n]*= : {token, {first, {TokenLine,TokenCol}}}.\n"
            "[a]+ : {token, {second, {TokenLine,TokenCol}}}.\n"
            "[\\s\\r\\n\\t]+ : skip_token.\n"
            "Erlang code.\n"
            "-export([t/0]).\n"
            "t() ->\n"
            "{ok,[{second,{1,1}}],{2,1}} = string(\"a\\n\"), ok.\n">>,
    ok = case catch run_test(Config, X1, default, [{tab_size,0}]) of
            {'EXIT',{badarg,_}} -> ok
        end,
    ok = case catch run_test(Config, X1, default, [{tab_size,what_is_tab}]) of
            {'EXIT',{badarg,_}} -> ok
        end,
    ok = case catch run_test(Config, X1, default, [{error_location,{line,column}}]) of
            {'EXIT',{badarg,_}} -> ok
        end,
    ok = case catch run_test(Config, X1, default, [{error_location,33}]) of
            {'EXIT',{badarg,_}} -> ok
        end.

token_loc_var(Config) ->
    Ts = [
        {token_loc1,
        <<"Definitions.\n"
            "Rules.\n"
            "[a]+[\\n]*= : {token, {first, TokenLoc}}.\n"
            "[a]+ : {token, {second, {TokenLine,TokenCol}}}.\n"
            "[\\s\\r\\n\\t]+ : skip_token.\n"
            "Erlang code.\n"
            "-export([t/0]).\n"
            "t() ->\n"
            "{ok,[{second,{1,9}}],{2,1}} = string(\"\ta\\n\"), ok.\n">>,
      default,
      [{error_location,column}],
      ok},
    {token_loc2,
        <<"Definitions.\n"
            "Rules.\n"
            "[a]+[\\n]*= : {token, {first, TokenLoc}}.\n"
            "[a]+ : {token, {second, TokenLoc}}.\n"
            "[\\s\\r\\n\\t]+ : skip_token.\n"
            "Erlang code.\n"
            "-export([t/0]).\n"
            "t() ->\n"
            "{ok,[{second,{1,15}},{second,{2,9}}],{2,16}} = string(\"   \t \t\t  a\\n \t \t  aaa\t\"), ok.\n">>,
        default,
        [{tab_size,3},{error_location,column}],
        ok}],
    run(Config, Ts),
    ok.