Leex column support.

`xrl` files for manually testing are located in `./examples`.

I use `column_SUITE` to test the column feature, while `leex_SUITE` contains standard tests for leex.
`leex_column_SUITE` test are identical to the `leex_SUITE`, but includes column numbers for error positions.

### Design
Column number in error messages is optional, specified with `column` option.
To allow backward compatibility and avoid code duplication, I always track the column number which is then filtered (if `column = false`) while reporting errors and warnings.

### Build

    $   rebar3 compile

### Test

    $   rebar3 ct --sname test --include include

TODO:
error_location :: column | line
token_location
tab
testiraj s više pluseva