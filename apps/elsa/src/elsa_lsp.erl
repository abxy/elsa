-module(elsa_lsp).

-export([start_link/1]).

-export([
    initialize/1,
    'textDocument/hover'/1
]).

start_link(Port) ->
    Pid = proc_lib:spawn_link(fun() -> start_server(Port) end),
    {ok, Pid}.

start_server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {active, true}]),
    {ok, Sock} = gen_tcp:accept(LSock),
    loop(Sock, <<>>).

loop(Sock, Buf) ->
    receive
        {respond, Id, Resp} ->
            send(Sock, Resp#{jsonrpc => <<"2.0">>, id => Id}),
            loop(Sock, Buf);
        {tcp, Sock, Data} ->
            {Msgs, Rest} = decode(<<Buf/binary, Data/binary>>),
            process_messages(Msgs),
            loop(Sock, Rest);
        {tcp_closed, Sock} -> ok;
        {tcp_error, Sock, _Reason} -> ok
    end.



send(Sock, Msg) ->
    MsgB = jsx:encode(Msg),
    Header = io_lib:fwrite("Content-Length: ~w\r\n\r\n", [size(MsgB)]),
    gen_tcp:send(Sock, [Header, MsgB]).


process_messages([]) ->
    ok;
process_messages([Msg|Msgs]) ->
    Parsed = jsx:decode(Msg, [return_maps, {labels, atom}]),
    process_msg(Parsed),
    process_messages(Msgs).


process_msg(#{jsonrpc := <<"2.0">>, method := Method} = Msg) ->
    MethodF = binary_to_atom(Method, utf8),
    case erlang:function_exported(?MODULE, MethodF, 1) of
        true -> ?MODULE:MethodF(Msg);
        false -> process_msg_unknown(Msg)
    end.

process_msg_unknown(#{id := Id} = Msg) ->
    respond_error(Id, method_not_found, <<"Method Not Found"/utf8>>),
    io:format("UNKNOWN REQUEST: ~p~n", [Msg]);
process_msg_unknown(Msg) ->
    io:format("UNKNOWN NOTIFICATION: ~p~n", [Msg]).

initialize(#{id := Id} = Msg) ->
    respond_result(Id, #{
        capabilities => #{
            hoverProvider => true
        }
    }),
    io:format("INITIALIZE: ~p~n", [Msg]).

'textDocument/hover'(Msg) ->
    #{params := #{
        position := #{
            character := Col,
            line := Line
        },
        textDocument := #{
            uri := Path
        }
    }} = Msg,
    io:format("HOVER: ~w:~w:~w~n", [Path, Line, Col]).


respond_result(Id, Result) ->
    respond(Id, #{result => Result}).

respond_error(Id, Code, Message) ->
    respond(Id, #{error =>
        #{
            code => error_code(method_not_found),
            message => Message
        }
    }).

respond(Id, Resp) ->
    self () ! {respond, Id, Resp}.

decode(Buf) ->
    decode(Buf, []).

decode(Buf, Msgs) ->
    case decode1(Buf, 0) of
        {Msg, Rest} -> decode(Rest, [Msg|Msgs]);
        more -> {lists:reverse(Msgs), Buf}
    end.

decode1(Buf, N) ->
    case erlang:decode_packet(httph, Buf, []) of
        {ok, {http_header, _, 'Content-Length', _, Len}, Rest} ->
            decode1(Rest, list_to_integer(Len));
        {ok, {http_header, _, _, _, _}, Rest} ->
            decode1(Rest, N);
        {ok, http_eoh, Rest} when size(Rest) < N ->
            more;
        {ok, http_eoh, <<Msg:N/binary, Rest/binary>>} ->
            {Msg, Rest};
        {more, _} ->
            more
    end.



% Defined by JSON RPC
error_code(parse_error) -> -32700;
error_code(invalid_request) -> -32600;
error_code(method_not_found) -> -32601;
error_code(invalid_params) -> -32602;
error_code(internal_error) -> -32603;
% Reserved range for server errors
%error_code(server_error_AAA) -> -32099;
%error_code(server_error_ZZZ) -> -32000;
error_code(server_not_initialized) -> -32002;
error_code(unknown_error_code) -> -32001;
% Defined by the language server protocol.
error_code(request_cancelled) -> -32800.