-ifdef(debug).
-define(config(X,Y), foo).
-define(datadir, "column_SUITE_data").
-define(privdir, "column_SUITE_priv").
-else.
-include_lib("common_test/include/ct.hrl").
-define(datadir, ?config(data_dir, Config)).
-define(privdir, ?config(priv_dir, Config)).
-endif.

run(Config, Tests) ->
    F = fun({N,P,Pre,E}) ->
                case catch run_test(Config, P, Pre, []) of
                    E -> 
                        ok;
                    Bad -> 
                        ct:fail("~nTest ~p failed. Expected~n  ~p~n"
                                  "but got~n  ~p~n", [N, E, Bad])
                end;
            ({N,P,Pre,Opts,E}) ->
                case catch run_test(Config, P, Pre,Opts) of
                    E -> 
                        ok;
                    Bad -> 
                        ct:fail("~nTest ~p failed. Expected~n  ~p~n"
                                  "but got~n  ~p~n", [N, E, Bad])
                end
        end,
    lists:foreach(F, Tests).

run_test(Config, Def, Pre) ->
    run_test(Config, Def, Pre, []).

run_test(Config, Def, Pre, LOpts0) ->
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
    LRet = leex:file(XrlFile, XOpts ++ LOpts++LOpts0),
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
