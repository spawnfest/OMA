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

-include_lib("stdlib/include/erl_compile.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/file.hrl").

-ifdef(debug).
-define(config(X,Y), foo).
-define(datadir, "column_SUITE_data").
-define(privdir, "column_SUITE_priv").
-else.
-include_lib("common_test/include/ct.hrl").
-define(datadir, ?config(data_dir, Config)).
-define(privdir, ?config(priv_dir, Config)).
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([
	 number/1]).

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
    [number].

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

run(Config, Tests) ->
    F = fun({N,P,Pre,E}) ->
                case catch run_test(Config, P, Pre) of
                    E -> 
                        ok;
                    Bad -> 
                        ct:fail("~nTest ~p failed. Expected~n  ~p~n"
                                  "but got~n  ~p~n", [N, E, Bad])
                end
        end,
    lists:foreach(F, Tests).

run_test(Config, Def, Pre) ->
    %% io:format("testing ~s~n", [binary_to_list(Def)]),
    DefFile = 'leex_test.xrl',
    Filename = 'leex_test.erl',
    DataDir = ?privdir,
    XrlFile = filename:join(DataDir, DefFile),
    ErlFile = filename:join(DataDir, Filename),
    Opts = [return, warn_unused_vars,{outdir,DataDir}],
    ok = file:write_file(XrlFile, Def),
    LOpts = [return, {report, false} | 
             case Pre of
                 default ->
                     [];
                 _ ->
                     [{includefile,Pre}]
             end],
    XOpts = [verbose, dfa_graph], % just to get some code coverage...
    LRet = leex:file(XrlFile, XOpts ++ LOpts),
    case LRet of
        {ok, _Outfile, _LWs} ->
                 CRet = compile:file(ErlFile, Opts),
                 case CRet of
                     {ok, _M, _Ws} -> 
                         AbsFile = filename:rootname(ErlFile, ".erl"),
                         Mod = leex_test,
                         code:purge(Mod),
                         code:load_abs(AbsFile, Mod),
                         Mod:t();
                         %% warnings(ErlFile, Ws);
                     {error, [{ErlFile,Es}], []} -> {error, Es, []};
                     {error, [{ErlFile,Es}], [{ErlFile,Ws}]} -> {error, Es, Ws};
                     Error  -> Error
                 end;
        {error, [{XrlFile,LEs}], []} -> {error, LEs, []};
        {error, [{XrlFile,LEs}], [{XrlFile,LWs}]} -> {error, LEs, LWs};
        LError -> LError
    end.

extract(File, {error, Es, Ws}) ->
    {errors, extract(File, Es), extract(File, Ws)};    
extract(File, Ts) ->
    lists:append([T || {F, T} <- Ts,  F =:= File]).

search_for_file_attr(PartialFilePathRegex, Forms) ->
    lists:search(fun
                   ({attribute, _, file, {FileAttr, _}}) ->
                      case re:run(FileAttr, PartialFilePathRegex, [unicode]) of
                        nomatch -> false;
                        _ -> true
                      end;
                   (_) -> false end,
                 Forms).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Additional tests %%%
%%%%%%%%%%%%%%%%%%%%%%%%

number(Config) ->
    Ts = [{integer,
      <<"Definitions.\n"
        "D = [0-9]\n"
        "Rules.\n"
        "{D}+ :\n"
        "{token,{integer,TokenLine,list_to_integer(TokenChars)}}.\n"
        "{D}+\\.{D}+((E|e)(\\+|\\-)?{D}+)? :\n"
        "{token,{float,TokenLine,list_to_float(TokenChars)}}.\n"
        "Erlang code.\n"
        "-export([t/0]).\n"
        "t() ->\n"
        "{ok,[{float, 1, 4.44},1} = string(\"4.44\"), ok.\n">>,
           default,
           ok}],
    run(Config, Ts),
    ok.