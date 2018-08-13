-module(buffer).
-compile([export_all, nowarn_export_all]).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-endif.

% Experiments in building a text buffer data structure
% for use in the elsa language server.
%
% It's based on specialized finger trees[1], with
% inspiration from the Yi editor's rope data structure[2].
%
% [1]: http://www.staff.city.ac.uk/~ross/papers/FingerTree.html
% [2]: https://github.com/yi-editor/yi-rope/blob/master/src/Yi/Rope.hs

-define(MERGE_CHUNK_SIZE, 16).
-define(CHUNK_SIZE, 3).

from_binary(Binary) ->
    from_binary(empty, Binary).

from_binary(Tr, <<>>) -> Tr;
from_binary(Tr, Binary) ->
    {Chunk, Rest} = take_chunk(?CHUNK_SIZE, Binary),
    from_binary(push_r(Tr, Chunk), Rest).

% Take a chunk of a specified byte size from a binary.
% If the desired size would result in a character
% being split, a larger chunk is taken instead.
%
take_chunk(Size, Chunk) when size(Chunk) =< Size ->
    {Chunk, <<>>};
take_chunk(Size, Binary) ->
    case Binary of
        <<_:Size/binary, _/utf8, _/binary>> ->
            <<Chunk:Size/binary, Rest/binary>> = Binary,
            {Chunk, Rest};
        _ ->
            take_chunk(Size + 1, Binary)
    end.

% Measures a single chunk of UTF8 encoded text.
%
% The measurement of a buffer is `{Line, Col, FirstIsLF, LastIsCR}`.
% `Line` is the 0 based line offset at the end, or viewed another way
% the number of line breaks.
% `Col` is the 0 based character offset at the end of the last line.
% Taken together they represent the (Line, Col) position of a cursor
% placed at the end of the buffer.
%
% `Col` follows the Language Server Protocol (LSP) specification in that
% it counts Unicode code points at U+10000 or higher as 2  characters.
% Recognized line breaks are "\n" (LF), "\r" (CR), and "\r\n" (CRLF).
%
% Adding the measurements of two buffers (cf. `size_add`) should yield
% the same answer as concatenating the buffers and measuring the result.
% In order to preserve a correct `Line` count, we need to know if
% concatenating the buffers would result in a CR and LF being joined
% to form a CRLF. This information is embedded in the two bools
% `FirstIsLF` which is true if the buffer starts with LF, and
% `LastIsCR` which is true if the buffer ends with CR.
%
measure_chunk(<<>>) -> undefined;
measure_chunk(Chunk) ->
    {Line, Col} = line_col(0, 0, Chunk),
    SizeSub1 = byte_size(Chunk) - 1,
    SizeSub2 = byte_size(Chunk) - 2,
    {FirstIsLF, LastIsCR} =
        case Chunk of
            <<"\n", _:SizeSub2/binary, "\r">> -> {true, true};
            <<"\n", _/binary>> -> {true, false};
            <<_:SizeSub1/binary, "\r">> -> {false, true};
            _ -> {false, false}
        end,
    {Line, Col, FirstIsLF, LastIsCR}.

% Calculate the {Line, Col} position at the end of the chunk.
%
line_col(Line, Col, <<>>) ->
    {Line, Col};
line_col(Line, _Col, <<"\n"/utf8, Rest/binary>>) ->
    line_col(Line+1, 0, Rest);
line_col(Line, _Col, <<"\r\n"/utf8, Rest/binary>>) ->
    line_col(Line+1, 0, Rest);
line_col(Line, _Col, <<"\r"/utf8, Rest/binary>>) ->
   line_col(Line+1, 0, Rest);
line_col(Line, Col, <<C/utf8, Rest/binary>>) when C >= 16#10000 ->
    % The LSP spec requires U+10000 and above to be counted as 2 characters
    line_col(Line, Col+2, Rest);
line_col(Line, Col, <<_/utf8, Rest/binary>>) ->
    line_col(Line, Col+1, Rest);
line_col(_, _, _) ->
    throw({error, invalid_utf8}).


size_add(undefined, Size) -> Size;
size_add(Size, undefined) -> Size;
size_add({Line, Col1, LF, _}, {0, Col2, _, _}) ->
    {Line, Col1 + Col2, LF, false};
size_add({Line1, _, LF, true}, {Line2, Col, true, CR}) ->
    % Deduct 1 line for the merged CRLF
    {Line1 + Line2 - 1, Col, LF, CR};
size_add({Line1, _, LF, _}, {Line2, Col, _, CR}) ->
    {Line1 + Line2, Col, LF, CR}.


measure(Chunk) when is_binary(Chunk) -> measure_chunk(Chunk);
measure({Size, _, _}) -> Size;
measure({Size, _, _, _}) -> Size.

measure_digit({A}) -> measure(A);
measure_digit({A, B}) -> size_add(measure(A), measure(B));
measure_digit({A, B, C}) -> size_add(measure(A), size_add(measure(B), measure(C)));
measure_digit({A, B, C, D}) -> size_add(measure(A), size_add(measure(B), size_add(measure(C), measure(D)))).

measure_tree(empty) -> undefined;
measure_tree({deep, Size, _, _, _}) -> Size;
measure_tree(Single) -> measure(Single).

% Smart constructors for 2,3-nodes.
% Caching the combined size of their contents.
%
node2(A, B) ->
    Size = size_add(measure(A), measure(B)),
    {Size, A, B}.

node3(A, B, C) ->
    Size = size_add(measure(A), size_add(measure(B), measure(C))),
    {Size, A, B, C}.

% Smart constructor for deep trees.
deep(Sf, M, Pr) ->
    Size = size_add(measure_digit(Sf), size_add(measure_tree(M), measure_digit(Pr))),
    {deep, Size, Sf, M, Pr}.

% Push from the left
push_l(A, empty) -> A;
push_l(A, {deep, _Sz, Pr, M, Sf})
        when byte_size(A) + byte_size(element(1, Pr)) =< ?MERGE_CHUNK_SIZE ->
    deep(setelement(1, Pr, <<A/binary, (element(1, Pr))/binary>>), M, Sf);
push_l(A, {deep, _Sz, {B, C, D, E}, M, Sf}) ->
    deep({A, B}, push_l(node3(C, D, E), M), Sf);
push_l(A, {deep, _Sz, Pr, M, Sf}) ->
    deep(erlang:insert_element(1, Pr, A), M, Sf);
push_l(A, B) when byte_size(A) + byte_size(B) =< ?MERGE_CHUNK_SIZE ->
    <<A/binary, B/binary>>;
push_l(A, B) ->
    deep({A}, empty, {B}).


% Push from the right
push_r(empty, A) -> A;
push_r({deep, _Sz, Pr, M, Sf}, A)
        when byte_size(A) + byte_size(element(tuple_size(Sf), Sf)) =< ?MERGE_CHUNK_SIZE ->
    deep(Pr, M, setelement(tuple_size(Sf), Sf, <<(element(tuple_size(Sf), Sf))/binary, A/binary>>));
push_r({deep, _Sz, Pr, M, {A, B, C, D}}, E) ->
    deep(Pr, push_r(M, node3(A, B, C)), {D, E});
push_r({deep, _Sz, Pr, M, Sf}, A) ->
    deep(Pr, M, erlang:append_element(Sf, A));
push_r(A, B) when byte_size(A) + byte_size(B) =< ?MERGE_CHUNK_SIZE ->
    <<A/binary, B/binary>>;
push_r(A, B) ->
    deep({A}, empty, {B}).

head_l({deep, _Sz, Pr, _M, _Sf}) -> element(1, Pr).

tail_l({deep, _Sz, Pr, M, Sf}) -> deep_l(erlang:delete_element(1, Pr), M, Sf).

view_l(empty) -> empty;
view_l({deep, _Sz, Pr, M, Sf}) ->
    {element(1, Pr), deep_l(erlang:delete_element(1, Pr), M, Sf)};
view_l(S) -> {S, empty}.

deep_l({}, M, Sf) ->
    case view_l(M) of
        empty -> digit_to_tree(Sf);
        {Hd, Tl} ->
            % The middle tree always stores 2,3-nodes, so we know
            % that Hd will be a tuple with 2 or 3 elements, thus
            % we can it as the digit prefix without conversion.
            deep(Hd, Tl, Sf)
    end;
deep_l(Pr, M, Sf) -> deep(Pr, M, Sf).

view_r(empty) -> empty;
view_r({deep, _Sz, Pr, M, Sf}) ->
    {deep_r(Pr, M, erlang:delete_element(tuple_size(Sf), Sf)), element(tuple_size(Sf), Sf)};
view_r(S) -> {S, empty}.

deep_r(Pr, M, {}) ->
    case view_r(M) of
        empty -> digit_to_tree(Pr);
        {Tl, Hd} ->
            % The middle tree always stores 2,3-nodes, so we know
            % that Hd will be a tuple with 2 or 3 elements, thus
            % we can it as the digit suffix without conversion.
            deep(Pr, Tl, Hd)
    end;
deep_r(Pr, M, Sf) -> deep(Pr, M, Sf).

% TODO: Make this more balanced
digit_to_tree({A}) -> A;
digit_to_tree(Digit) ->
    deep({element(1, Digit)}, empty, erlang:delete_element(1, Digit)).

% `From` and `To` are inclusive.
%
digit_to_tree(Digit, From, To) ->
    digit_to_tree(Digit, From, To, empty).

digit_to_tree(Digit, I, To, Tree) when I =< To ->
    digit_to_tree(Digit, I + 1, To, push_l(element(I, Digit), Tree));
digit_to_tree(_, _, _, Tree) -> Tree.

split_tree(Pred, Acc, {deep, _Sz, Pr, M, Sf}) ->
    AccPr = size_add(Acc, measure_digit(Pr)),
    case Pred(AccPr) of
        true ->
            I = split_digit(Pred, AccPr, Pr),
            L = digit_to_tree(Pr, 1, I - 1),
            R = take_r_digit(Pr, tuple_size(Pr) - I),
            {L, element(I, Pr), deep_l(R, M, Sf)};
        false ->
            AccPrM = size_add(AccPr, measure_tree(M)),
            case Pred(AccPrM) of
                true ->
                    {ML, Xs, MR} = split_tree(Pred, AccPr, M),
                    AccPrML = size_add(AccPr, measure_tree(ML)),
                    % Since we recursed Xs must be a 2,3-node
                    % which can be treated as a digit
                    I = split_digit(Pred, AccPrML, Xs),
                    L = take_l_digit(Xs, I - 1),
                    R = take_r_digit(Xs, tuple_size(Xs) - I),
                    {deep_r(Pr, ML, L), element(I, Xs), deep_l(R, MR, Sf)};
                false ->
                    I = split_digit(Pred, AccPrM, Sf),
                    R = digit_to_tree(Sf, I + 1, tuple_size(Sf)),
                    L = take_l_digit(Pr, I - 1),
                    {deep_r(Pr, M, L), element(I, Sf), R}
            end
    end;
split_tree(_Pred, _Acc, Single) -> {empty, Single, empty}.

% Finds the index of the split point in a digit.
%
split_digit(Pred, Acc, Digit) ->
    split_digit(Pred, Acc, Digit, 1).

split_digit(Pred, Acc, Digit, I) when I < tuple_size(Digit) ->
    Acc1 = size_add(Acc, measure(element(1, Digit))),
    case Pred(Acc1) of
        true -> I;
        false -> split_digit(Pred, Acc1, Digit, I + 1)
    end;
split_digit(_Pred, _Acc, _Digit, I) -> I.

take_r_digit(Digit, N) when N > tuple_size(Digit) ->
    throw({error, take_r_digit});
take_r_digit(_Digit, 0) -> {};
take_r_digit(Digit, 1) ->
    {element(tuple_size(Digit), Digit)};
take_r_digit(Digit, 2) ->
    N = tuple_size(Digit),
    {element(N-1, Digit), element(N, Digit)};
take_r_digit(Digit, 3) ->
    N = tuple_size(Digit),
    {element(N-2, Digit), element(N-1, Digit), element(N, Digit)};
% Must be whole digit since 4 is the maxim length
take_r_digit(Digit, 4) -> Digit.


take_l_digit(Digit, N) when N > tuple_size(Digit) ->
    throw({error, take_l_digit});
take_l_digit(_Digit, 0) -> {};
take_l_digit(Digit, 1) -> {element(1, Digit)};
take_l_digit(Digit, 2) -> {element(1, Digit), element(2, Digit)};
take_l_digit(Digit, 3) -> {element(1, Digit), element(2, Digit), element(3, Digit)};
 % Must be whole digit since 4 is the maxim length
take_l_digit(Digit, 4) -> Digit.


%-------------------------------------------------------------------------------
% Property tests

-ifdef(TEST).

prop_measure_from_binary() ->
    ?FORALL(Bin, utf8(), measure(Bin) =:= measure_tree(from_binary(Bin))).

-endif.