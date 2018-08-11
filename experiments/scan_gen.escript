#!/usr/bin/env escript

% The idea with this file is to be able to generate specialized scanners
% for different input formats and data structures.

-import(io_lib, [write_char/1, write_string/1, write_atom/1]).

-record(cfg, {
    binary = true
}).

main(_) ->
    Code = gen_scan1(#cfg{}),
    io:format(Code).

punctuation() ->
    ["<<", "<-", "<=", "<", ">>", ">=", ">", "--", "-", "->", "++", "+",
     "=:=", "=/=", "==", "=<", "...", "..", "."].

% These are punctuation tokens that are also prefixes of
% some other token thus necessetating the need for looking ahead.
lookahead_punctuation() ->
    [P || P <- punctuation(), Other <- punctuation(), P =:= Other, lists:prefix(P, Other)].

gen_scan1(Cfg) ->
    [gen_token_scan1([C], Cfg) || C <- ",(){}[];"] ++
    [gen_token_scan1(X, Cfg) || X <- punctuation()].

gen_token_scan1(X, Cfg) when Cfg#cfg.binary ->
    ["scan1(<<", write_string(X), ",Cs/binary>>, St, Line, Col, Toks) -> \n",
     "  tok2(Cs, St, Line, Col, Toks, ", write_string(X), ", ", write_atom(list_to_atom(X)), ");\n"].