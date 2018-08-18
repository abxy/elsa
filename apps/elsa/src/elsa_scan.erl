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
    scan(Str ++ Rest, Next, Toks) ->
        Tok = {list_to_atom(Str), length(Str)},
        scan(Rest, Next, [Tok|Toks])
).

-define(scan_more(Str),
    scan(Str, Next, Toks) when Next =/= eof ->
        {more, fun (More, Next1) -> scan(Str ++ More, Next1, Toks) end};
    ?scan(Str)
).

scan([C|_] = Cs, Next, Toks) when C >= 0, C =< 32 ->
    scan_white_space(Cs, Next, Toks, 0, 0);
scan([C|Cs], Next, Toks) when C >= $a, C =< $z ->
    scan_atom(Cs, Next, Toks, 1, [C]);
scan([C|Cs], Next, Toks) when C >= $A, C =< $Z;
                              C == $_ ->
    scan_variable(Cs, Next, Toks, 1, [C]);
scan([C|Cs], Next, Toks) when ?DIGIT(C) ->
    scan_number(Cs, Next, Toks, 1, [C]);
scan("%" ++ Cs, Next, Toks) ->
    scan_comment(Cs, Next, Toks, 1);
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
?scan_more(":");
?scan("#");
?scan("!");
?scan("*");
scan([], Next, Toks) when Next =/= eof ->
    {more, fun (More, Next1) -> scan(More, Next1, Toks) end};
scan([], eof, Toks) ->
    {ok, lists:reverse(Toks)}.

scan_number([C|Cs], Next, Toks, Col, Ncs) when ?DIGIT(C) ->
    scan_number(Cs, Next, Toks, Col + 1, [C|Ncs]);
scan_number([$#|Cs], Next, Toks, Col, Ncs) ->
    Base = list_to_integer(lists:reverse(Ncs)),
    if
        Base >= 2, Base =< 36 ->
            scan_based_int(Cs, Next, Toks, Col+1, Base, []);
        true ->
            {error, {illegal, base}}
    end;
scan_number([], Next, Toks, Col, Ncs) when Next =/= eof ->
    {more, fun (More, Next1) -> scan_number(More, Next1, Toks, Col, Ncs) end};
scan_number(Cs, Next, Toks, Col, Ncs) ->
    Val = list_to_integer(lists:reverse(Ncs)),
    Tok = {int, Col, Val},
    scan(Cs, Next, [Tok|Toks]).

-define(DIGIT_IN_BASE(C, Base),
    ((C >= $0 andalso C - $0 < Base) orelse
    (C >= $a andalso C - $a + 10 < Base) orelse
    (C >= $A andalso C - $A + 10 < Base))).

scan_based_int([C|Cs], Next, Toks, Col, Base, Ncs) when ?DIGIT_IN_BASE(C, Base) ->
    scan_based_int(Cs, Next, Toks, Col + 1, Base, [C|Ncs]);
scan_based_int([], Next, Toks, Col, Base, Ncs) when Next =/= eof ->
    {more, fun (More, Next1) -> scan_based_int(More, Next1, Toks, Col, Ncs, Base) end};
scan_based_int(Cs, Next, Toks, Col, Base, Ncs) ->
    Val = list_to_integer(lists:reverse(Ncs), Base),
    Tok = {int, Col, Val},
    scan(Cs, Next, [Tok|Toks]).

scan_variable(Cs, Next, Toks, Col, Wcs) ->
    case scan_name(Cs, Next, Col, Wcs) of
        {more, Col1, Wcs1} ->
            {more, fun (More, Next1) -> scan_atom(Cs ++ More, Next1, Toks, Col1, Wcs1) end};
        {ok, Col1, Wcs1, Rest} ->
            Tok = {var, Col1, lists:reverse(Wcs1)},
            scan(Rest, Next, [Tok|Toks])
    end.

scan_atom(Cs, Next, Toks, Col, Wcs) ->
    case scan_name(Cs, Next, Col, Wcs) of
        {more, Col1, Wcs1} ->
            {more, fun (More, Next1) -> scan_atom(More, Next1, Toks, Col1, Wcs1) end};
        {ok, Col1, Wcs1, Rest} ->
            Name = list_to_atom(lists:reverse(Wcs1)),
            Tok = case reserved_word(Name) of
                true -> {Name, Col1};
                false -> {atom, Col1, Name}
            end,
            scan(Rest, Next, [Tok|Toks])
    end.

scan_name([C|Cs], Next, Col, Wcs) when C >= $a, C =< $z
                                     ; C >= $A, C =< $Z
                                     ; C >= $ß, C =< $ÿ, C =/= $÷
                                     ; C >= $À, C =< $Þ, C =/= $× ->
    scan_name(Cs, Next, Col + char_col_size(C), [C|Wcs]);
scan_name([], Next, Col, Wcs) when Next =/= eof ->
    {more, Col, Wcs};
scan_name(Rest, _Next, Col, Wcs) ->
    {ok, Col, Wcs, Rest}.

scan_comment([C|_] = Cs, Next, Toks, Col) when C == $\n orelse C == $\r ->
    Tok = {comment, Col},
    scan_white_space(Cs, Next, [Tok|Toks], 0, 0);
scan_comment([C|Cs], Next, Toks, Col) ->
    scan_comment(Cs, Next, Toks, Col + char_col_size(C));
scan_comment([], Next, Toks, Col) when Next =/= eof ->
    {more, fun (More, Next1) -> scan_comment(More, Next1, Toks, Col) end};
scan_comment(Cs, Next, Toks, Col) ->
    Tok = {comment, Col},
    scan(Cs, Next, [Tok|Toks]).

scan_white_space("\n" ++ Cs, Next, Toks, Line, _Col) ->
    scan_white_space(Cs, Next, Toks, Line + 1, 0);
scan_white_space("\r\n" ++ Cs, Next, Toks, Line, _Co) ->
    scan_white_space(Cs, Next, Toks, Line + 1, 0);
scan_white_space("\r", Next, Toks, Line, _Col) when Next =/= eof ->
    {more, fun (More, Next1) ->
        scan_white_space("\r" ++ More, Next1, Toks, Line, 0)
    end};
scan_white_space("\r" ++ Cs, Next, Toks, Line, _Cols) ->
    scan_white_space(Cs, Next, Toks, Line + 1, 0);
scan_white_space([C|Cs], Next, Toks, Line, Col) when ?WHITE_SPACE(C) ->
    scan_white_space(Cs, Next, Toks, Line, Col + char_col_size(C));
scan_white_space(Cs, Next, Toks, Line, Col) ->
    Tok = {white_space, {Line, Col}},
    scan(Cs, Next, [Tok|Toks]).

-compile({inline, [char_col_size/1]}).

-spec char_col_size(Char :: char()) -> integer().
char_col_size(C) ->
    if
        C < 16#10000 -> 1;
        true -> 2
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
