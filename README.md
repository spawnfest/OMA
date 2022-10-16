# Leex column support.

[Spawnfest2022](https://spawnfest.org/) project.

Author: marko.mindek@gmail.com

The idea is taken from the Spawnfest project ideas:

    Currently Leex https://erlang.org/doc/man/leex.html the lexical analyser generator for Erlang does not support column numbers for the generated scanners. 
    The returned tokens contain only a line number and not a tuple of {line(), column()}.
    This doesn't matter for the Erlang compiler because it doesn't use a leex generated scanner but all languages that do use leex can't support column numbers in error messages because of that.
    For backwards compatibility this should probably be optional.
    Optimally such a project could target itself for inclusion in Erlang/OTP. 

This is a *forked* part of the latest OTP's `leex` module.
Two source files that are relevant (`leexinc.hrl` and `leex.erl`) and were taken from the latest OTP. All modification of those 2 files are in one [commit](https://github.com/spawnfest/OMA/commit/9eece882a35590c1a68145bc3310d3982392e493) so it is easy to follow it.
All the modifications were done by myself and everything was done inside hackaton time interval.

### Main Features
+   Column number in errors
+   Column number in tokens
    +   **This can be very useful if you want to include token's column number in the `yecc`-generated parser!**
+   Configurable tab lenght

## Design
Errors in error messages can now return either `Line` or `{Line, Column}`. This can be configured by passing option `{error_location, line | column}` to `leex:file/2`. Default value is `line`. To allow backward compatibility and avoid code duplication, column number is always tracked and then filtered if needed. This also allows us to set the column number in tokens, even if the `error_location` is `line`.

If `error_location` is set to `column`, then functions returning `EndLoc` also return `{Line, Col}`. If it's set to default, `EndPos` is only `Line`.

To clarify, `error_location` and location of the token are not depenend in any way! You can use `{TokenLine, TokenCol}` in your tokens while `error_location` is `line`. Also, you can use `TokenLine` while `error_location` is `column`, etc.

Option `{tab_size, pos_integer()}` is also added. This allows user to customize tab length. Default is `8`.

### Added Files

+   **examples/\***
    +   .xrl file examples for manual testing
    +   intructions on how to try one are in the section Demo
+   **test/**   *(In reality this should be one file ofc.)*
    +   **tutil.hrl**
        +   test runner functions
    +   **column_SUITE.erl**
        +   tests that test only added features
    +   **leex_SUITE**
        +   official test from OTP
        +   to make sure I don't break backward compatibilty
    +   **leex_column_SUITE**
        +   relevant tests from the `leex_SUITE`, but with columns
+   rebar.config
+   .gitignore
+   my_leex.app.src
+   ts_install_cth.erl
    +   I've added this here to enable CTH locally (this can be dissmissed).

### Added functions

Two new function are exported: `token/4` and `tokens/4`. This is to allow those function to start tokenizing not just from some line, but from some line and column. Rule of thumb is: when `error_location` is `line`, use `token/3` and `token/4`. When it is `column`, use `token/4` and `tokens/4`.

### Added variables

I've added two new variables, `TokenCol` and `TokenLoc`, to be available in writing `Actions` alongside `TokenChars`, `TokenLen` and `TokenLine`. `TokenCol` represents starting column of the token. Variable `TokenLoc` is just an alias for `{TokenLine, TokenCol}` to save some characters while writing `.xrl` files.

## Build
Environment: rebar 3.19.0 on Erlang/OTP 25 Erts 13.0

    rebar3 compile

## Test

    rebar3 ct --sname test

## Demo

    rebar3 shell

Even though it is undefined, modified `leex` module should be loaded by rebar3. I didn't want to modify the module's name for this case, this should be enough for testing.

Lets assume that you have your `my_example.xrl` file in `examples/` directory. You can generate a lexical analyzer from it in a following way:

    1> code:purge(my_example)
    2> leex:file("examples/my_example",[])
    3> compile:file("examples/my_example.erl",[{outdir, "./examples/"}])
    4> code:load_abs("examples/my_example)

This will create the analyzer's source (*"examples/my_example.erl"*), compile it to the bytecode (*examples/my_example.beam*) and then load it. One can then interact with the loaded module by calling `string/1,2`, `token/2,3,4` or `tokens/2,3,4`.

Feel free to add options while calling `leex:file/2`, **but** make sure that you compiled and loaded the analyzer module again!

## TODOs

+ how to handle pushing characters back? Is it even possible to handle this while keeping Line and Column correctness?
+ more testing (especially unicode characters and column number correctness)
+ better documentation