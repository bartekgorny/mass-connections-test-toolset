-module(echo_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/3]).

start_link(Ref, _Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, _Opts = []) ->
    {ok, Socket} = ranch:handshake(Ref),
    gen_server:cast(counter, connection_open),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, 100000) of
        {ok, Data} ->
            gen_server:cast(counter, message_received),
            Transport:send(Socket, Data),
            loop(Socket, Transport);
        _ ->
            gen_server:cast(counter, connection_closed),
            ok = Transport:close(Socket)
    end.
