-module(elsa_scan).
-compile([export_all, nowarn_export_all]).

-define(WHITE_SPACE(C),
        is_integer(C) andalso
         (C >= $\000 andalso C =< $\s orelse C >= $\200 andalso C =< $\240)).
-define(DIGIT(C), C >= $0, C =< $9).
-define(CHAR(C), is_integer(C), C >= 0).
-define(UNICODE(C),
        is_integer(C) andalso
         (C >= 0 andalso C < 16#D800 orelse
          C > 16#DFFF andalso C < 16#FFFE orelse
          C > 16#FFFF andalso C =< 16#10FFFF)).

-define(scan(Str),
    scan(<<Str/utf8, Rest/binary>>, Next, Toks) ->
        Tok = {list_to_atom(Str), length(Str)},
        scan(Rest, Next, [Tok|Toks])
).

-define(scan_more(Str),
    scan(<<Str/utf8>> = Cs, Next, Toks) when Next =/= eof ->
        {more, fun (More, Next1) -> scan(<<Cs/binary, More/binary>>, Next1, Toks) end};
    ?scan(Str)
).

scan(<<C, Rest/binary>>, Next, Toks) when C >= 0, C =< 32 ->
    skip_white_space(Rest, Next, Toks, 0, 0);
scan(<<C, _/binary>> = Cs, Next, Toks) when C >= $a, C =< $z ->
    scan_atom(Cs, Next, Toks, 0, 0, Cs);
scan(<<C, _/binary>> = Cs, Next, Toks) when C >= $A, C =< $Z ->
    scan_variable(Cs, Next, Toks, 0, 0, Cs);
?scan(",");
?scan(";");
?scan("(");
?scan(")");
?scan("{");
?scan("}");
?scan("[");
?scan("]");
?scan("->");
?scan("--");
?scan_more("-");
?scan("||");
?scan_more("|");
?scan("==");
?scan("=<");
?scan("=>");
?scan("=:=");
?scan_more("=:"); % illegal
?scan("=/=");
?scan_more("=/"); % illegal
?scan_more("=");
?scan("/=");
?scan_more("/");
?scan("<<");
?scan("<=");
?scan("<-");
?scan_more("<");
?scan(">>");
?scan(">=");
?scan_more(">");
?scan("++");
?scan_more("+");
?scan("...");
?scan_more("..");
?scan_more(".");
?scan(":=");
?scan("::");
?scan(":");
?scan_more(":");
?scan("#");
?scan("!");
?scan("*");
scan(<<>>, Next, Toks) when Next =/= eof ->
    {more, fun (More, Next1) -> scan(More, Next1, Toks) end};
scan(<<>>, eof, Toks) ->
    {ok, lists:reverse(Toks)}.

scan_variable(Cs, Next, Toks, Col, Bytes, Head) ->
    case scan_name(Cs, Next, Col, Bytes) of
        {more, Col1, Bytes1} ->
            {more, fun (More, Next1) -> scan_atom(More, Next1, Toks, Col1, Bytes1, [Head|More]) end};
        {ok, Col1, Bytes1, Rest} ->
            Name = iolist_take(Head, Bytes1),
            Tok = {var, Col1, Name},
            scan(Rest, Next, [Tok|Toks])
    end.

scan_atom(Cs, Next, Toks, Col, Bytes, Head) ->
    case scan_name(Cs, Next, Col, Bytes) of
        {more, Col1, Bytes1} ->
            {more, fun (More, Next1) -> scan_atom(More, Next1, Toks, Col1, Bytes1, [Head|More]) end};
        {ok, Col1, Bytes1, Rest} ->
            Name = iolist_take(Head, Bytes1),
            Tok = case reserved_word(Name) of
                true -> {Name, Col1};
                false -> {atom, Col1, Name}
            end,
            scan(Rest, Next, [Tok|Toks])
    end.

scan_name(<<C, Rest/binary>>, Next, Col, Bytes) when C >= $a, C =< $z
                                                   ; C >= $A, C =< $Z ->
    scan_name(Rest, Next, Col + 1, Bytes + 1);
scan_name(<<C, Rest/binary>>, Next, Col, Bytes) when C >= $ß, C =< $ÿ, C =/= $÷
                                                   ; C >= $À, C =< $Þ, C =/= $× ->
    scan_name(Rest, Next, Col + char_col_size(C), Bytes + char_byte_size(C));
scan_name(<<>>, Next, Col, Bytes) when Next =/= eof ->
    {more, Col, Bytes};
scan_name(Rest, _Next, Col, Bytes) ->
    {ok, Col, Bytes, Rest}.

iolist_take(_Ls, 0) ->
    <<>>;
iolist_take(Bin, Len) when is_binary(Bin) ->
    <<X:Len/binary, _/binary>> = Bin,
    X;
iolist_take([L|Ls], Len) ->
    H = iolist_take(L, Len),
    <<H/binary, (iolist_take(Ls, Len - byte_size(H)))/binary>>.

skip_white_space(<<"\n"/utf8, Rest/binary>>, Next, Toks, Line, _Col) ->
    skip_white_space(Rest, Next, Toks, Line + 1, 0);
skip_white_space(<<"\r\n"/utf8, Rest/binary>>, Next, Toks, Line, _Col) ->
    skip_white_space(Rest, Next, Toks, Line + 1, 0);
skip_white_space(<<"\r"/utf8>> = Cs, Next, Toks, Line, _Col) when Next =/= eof ->
    {more, fun (More, Next1) ->
        skip_white_space(<<Cs/binary, More>>, Next1, Toks, Line, 0)
    end};
skip_white_space(<<"\r"/utf8, Rest/binary>>, Next, Toks, Line, _Col) ->
    skip_white_space(Rest, Next, Toks, Line + 1, 0);
skip_white_space(<<C, Rest/binary>>, Next, Line, Col, Toks) when ?WHITE_SPACE(C) ->
    skip_white_space(Rest, Next, Toks, Line, Col + 1);
skip_white_space(Cs, Next, Toks, _Line, _Col) ->
    scan(Cs, Next, Toks).

-compile({inline, [char_col_size/1, char_byte_size/1]}).

-spec char_col_size(Char :: char()) -> integer().
char_col_size(C) ->
    if
        C < 16#10000 -> 1;
        true -> 2
    end.

-spec char_byte_size(Char :: char()) -> integer().
char_byte_size(C) ->
    if
        C =< 16#7f -> 1;
        C =< 16#7ff -> 2;
        C =< 16#ffff -> 3;
        true -> 4
    end.

-spec reserved_word(Atom :: atom()) -> boolean().
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
reserved_word(_) -> false.
