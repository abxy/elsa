-module(buffer).
-compile([export_all, nowarn_export_all]).

% Experiments in building a text buffer data structure
% for use in the elsa language server.
%
% It's based on specialized finger trees[1], with
% inspiration from the Yi editor's rope data structure[2].
%
% [1]: http://www.staff.city.ac.uk/~ross/papers/FingerTree.html
% [2]: https://github.com/yi-editor/yi-rope/blob/master/src/Yi/Rope.hs

-define(MERGE_CHUNK_SIZE, 1).
-define(CHUNK_SIZE, 1).

from_binary(Binary) ->
    from_binary(<<>>, Binary).

from_binary(Tr, <<>>) -> Tr;
from_binary(Tr, Binary) ->
    {Chunk, Rest} = take_chunk(?CHUNK_SIZE, Binary),
    from_binary(pushr(Tr, Chunk), Rest).

% Take a chunk of a specified size from a binary.
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

% Push from the left
pushl(A, <<>>) -> A;
pushl(A, {deep, Pr, M, Sf})
        when byte_size(A) + byte_size(element(1, Pr)) =< ?MERGE_CHUNK_SIZE ->
    {deep, setelement(1, Pr, <<A/binary, (element(1, Pr))/binary>>), M, Sf};
pushl(A, {deep, {B, C, D, E}, M, Sf}) ->
    {deep, {A, B}, pushl({C, D, E}, M), Sf};
pushl(A, {deep, Pr, M, Sf}) ->
    {deep, erlang:insert_element(1, Pr, A), M, Sf};
pushl(A, B) when byte_size(A) + byte_size(B) =< ?MERGE_CHUNK_SIZE ->
    <<A/binary, B/binary>>;
pushl(A, B) ->
    {deep, {A}, <<>>, {B}}.

% Push from the right
pushr(<<>>, A) -> A;
pushr({deep, Pr, M, Sf}, A)
        when byte_size(A) + byte_size(element(tuple_size(Sf), Sf)) =< ?MERGE_CHUNK_SIZE ->
    {deep, Pr, M, setelement(tuple_size(Sf), Sf, <<(element(tuple_size(Sf), Sf))/binary, A/binary>>)};
pushr({deep, Pr, M, {A, B, C, D}}, E) ->
    {deep, Pr, pushr(M, {A, B, C}), {D, E}};
pushr({deep, Pr, M, Sf}, A) ->
    {deep, Pr, M, erlang:append_element(Sf, A)};
pushr(A, B) when byte_size(A) + byte_size(B) =< ?MERGE_CHUNK_SIZE ->
    <<A/binary, B/binary>>;
pushr(A, B) ->
    {deep, {A}, <<>>, {B}}.

