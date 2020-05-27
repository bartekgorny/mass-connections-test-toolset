%%%-------------------------------------------------------------------
%%% @author bartlomiejgorny
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. May 2020 13:25
%%%-------------------------------------------------------------------
-module(dummy_launcher).
-author("bartlomiejgorny").
%% API
-export([start/0]).

prt(X) ->
    io:format("~p~n", [X]).


start() ->
    Opts = application:get_all_env(dummyclient),
    Max = proplists:get_value(max_conns, Opts),

    start(1, Max, Opts),
    ok.

start(Max, Max, _Opts) ->
    ok;
start(Counter, Max, Opts) ->
    Step = proplists:get_value(report_step, Opts),
    case Counter rem Step of
        0 ->
            logger:notice("Conns: ~p", [Counter]),
            report({running, Counter});
        _ -> ok
    end,
    Me = self(),
    ArriveInt = proplists:get_value(arrive_interval, Opts),
    spawn_link(fun() -> start_client(Me, Opts) end),
    receive
        {over, Reason} ->
            logger:error("Stopping because of: ~p", [Reason]),
            report({over, Reason, Counter}),
            ok
        after ArriveInt ->
            start(Counter + 1, Max, Opts)
    end.


start_client(Main, Opts) ->
    Host = proplists:get_value(host, Opts),
    Port = proplists:get_value(port, Opts),
    MsgInt = proplists:get_value(msg_interval, Opts),
    case gen_tcp:connect(Host, Port, [{active, false}, {packet, 2}], 5000) of
        {ok, Sock} ->
            loop(Main, Sock, MsgInt);
        {error, E} ->
            logger:error("Connection failed: ~p", [E]),
            Main ! {over, {conn,  E }}
    end.

loop(Main, Sock, MsgInt) ->
    case gen_tcp:send(Sock, "hej") of
        ok ->
            case gen_tcp:recv(Sock, 0, 5000) of
                {ok, _} ->
                    timer:sleep(MsgInt),
                    loop(Main, Sock, MsgInt);
                {error, E} ->
                    Main ! {over, {recv, E}},
                    logger:error("Recv failed: ~p", [E])
            end;
        {error, E} ->
            Main ! {over, {send, E}},
            logger:error("Send failed: ~p", [E])
    end.

report(Info) ->
    case global:whereis_name(monmon) of
        undefined ->
            ok;
        Mon ->
            gen_server:cast(Mon, {report, node(), Info})
    end.
