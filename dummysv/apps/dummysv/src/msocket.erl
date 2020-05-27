%%%-------------------------------------------------------------------
%%% @author bartlomiejgorny
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. May 2020 12:41
%%%-------------------------------------------------------------------
-module(msocket).
-author("bartlomiejgorny").
%% API
-export([start/4]).

prt(X) ->
%%    logger:notice("~p", [X]).
    io:format("~p~n", [X]).



start(_Module, gen_tcp, Socket, _Opts) ->
    gen_server:cast(counter, connection_open),
    Pid = spawn(fun() -> loop(Socket) end),
    gen_tcp:controlling_process(Socket, Pid),
    ok.


loop(S) ->
    inet:setopts(S, [{active, once}]),
    receive
        {tcp, S, Data} ->
            Answer = process(Data),
            gen_server:cast(counter, message_received),
            gen_tcp:send(S, Answer),
            loop(S);
        {tcp_closed, S} ->
            gen_server:cast(counter, connection_closed),
            ok
    end.


process(Data) ->
    Data.
