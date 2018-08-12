-module(buffer).
-compile([export_all, nowarn_export_all]).

-include_lib("proper/include/proper.hrl").

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

measure_chunk(<<>>) -> undefined;
measure_chunk(Chunk) ->
    {Lines, Chars} = measure_chunk(0, 0, Chunk),
    SizeSub1 = byte_size(Chunk) - 1,
    SizeSub2 = byte_size(Chunk) - 2,
    {FirstIsLF, LastIsCR} =
        case Chunk of
            <<"\n", _:SizeSub2/binary, "\r">> -> {true, true};
            <<"\n", _/binary>> -> {true, false};
            <<_:SizeSub1/binary, "\r">> -> {false, true};
            _ -> {false, false}
        end,
    {Lines, Chars, FirstIsLF, LastIsCR}.


measure_chunk(Lines, Chars, <<>>) ->
    {Lines, Chars};
measure_chunk(Lines, Chars, <<"\n"/utf8, Rest/binary>>) ->
    measure_chunk(Lines+1, Chars+1, Rest);
measure_chunk(Lines, Chars, <<"\r\n"/utf8, Rest/binary>>) ->
    measure_chunk(Lines+1, Chars+1, Rest);
measure_chunk(Lines, Chars, <<"\r"/utf8, Rest/binary>>) ->
   measure_chunk(Lines+1, Chars+1, Rest);
measure_chunk(Lines, Chars, <<C/utf8, Rest/binary>>) when C >= 16#10000 ->
    % The LSP spec requires U+10000 and above to be counted as 2 characters
    measure_chunk(Lines, Chars+2, Rest);
measure_chunk(Lines, Chars, <<_/utf8, Rest/binary>>) ->
    measure_chunk(Lines, Chars+1, Rest);
measure_chunk(_, _, _) ->
    throw({error, invalid_utf8}).

size_add(undefined, Size) -> Size;
size_add(Size, undefined) -> Size;
size_add({Ls1, Cs1, LF, true}, {Ls2, Cs2, true, CR}) ->
    % Deduct 1 line for the merged CRLF
    {Ls1 + Ls2 - 1, Cs1 + Cs2, LF, CR};
size_add({Ls1, Cs1, LF, _}, {Ls2, Cs2, _, CR}) ->
    {Ls1 + Ls2, Cs1 + Cs2, LF, CR}.

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

digit_to_tree({A}) -> A;
digit_to_tree(Digit) ->
    deep({element(1, Digit)}, empty, erlang:delete_element(1, Digit)).

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


%-------------------------------------------------------------------------------
% Property tests

prop_measure_from_binary() ->
    ?FORALL(Bin, binary(), (catch measure(Bin)) =:= (catch measure_tree(from_binary(Bin)))).
