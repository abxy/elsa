-module(bench).
-compile(export_all).

bench(File) ->
    {ok, Data} = file:read_file(File),
    N = 1000,
    M1 = timer:tc(?MODULE, bench_str, [Data, N]),
    M2 = timer:tc(?MODULE, bench_bin, [Data, N]),
    {M1, M2}.

bench_str(Data, N) ->
    [meas_str(Data, X, X) || X <- lists:seq(1, N)],
    ok.

meas_str(Data, Line, Col) ->
    Str = unicode:characters_to_list(Data, utf8),
    measure_str(Str, Line, Col).

measure_str([], Line, Col) ->
    {Line, Col};
measure_str("\n" ++ [Cs], Line, _Col) ->
    measure_str(Cs, Line + 1, 0);
measure_str("\r\n" ++ [Cs], Line, _Col) ->
    measure_str(Cs, Line + 1, 0);
measure_str("\r" ++ [Cs], Line, _Col) ->
    measure_str(Cs, Line + 1, 0);
measure_str([C|Cs], Line, Col) ->
    measure_str(Cs, Line, Col + char_col_size(C)).

bench_bin(Data, N) ->
    [measure_bin(Data, X, X) || X <- lists:seq(1, N)],
    ok.

measure_bin(<<>>, Line, Col) ->
    {Line, Col};
measure_bin(<<"\n"/utf8, Cs/binary>>, Line, _Col) ->
    measure_bin(Cs, Line + 1, 0);
measure_bin(<<"\r\n"/utf8, Cs/binary>>, Line, _Col) ->
    measure_bin(Cs, Line + 1, 0);
measure_bin(<<"\r"/utf8, Cs/binary>>, Line, _Col) ->
    measure_bin(Cs, Line + 1, 0);
measure_bin(<<C/utf8, Cs/binary>>, Line, Col) ->
    measure_bin(Cs, Line, Col + char_col_size(C)).

-compile({inline, [char_col_size/1]}).

-spec char_col_size(Char :: char()) -> integer().
char_col_size(C) ->
    if
        C < 16#10000 -> 1;
        true -> 2
    end.