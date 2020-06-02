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
    ArriveInt = proplists:get_value(arrive_interval, Opts),
    timer:send_interval(ArriveInt, new_connection),

    start(1, Max, Opts, #{}),
    ok.

start(Max, Max, _Opts, _Errs) ->
    ok;
start(Counter, Max, Opts, Errs) ->
    receive
        {failed, Reason} ->
            logger:error("Failed because of: ~p", [Reason]),
	    Errs1 = maps:put(Reason, maps:get(Reason, Errs, 0) + 1, Errs),
            report({running, Counter, Errs}),
	    start(Counter - 1, Max, Opts, Errs1),
            ok;
        {stopped, Reason} ->
            logger:error("Stopping because of: ~p", [Reason]),
	    Errs1 = maps:put(Reason, maps:get(Reason, Errs, 0) + 1, Errs),
            report({running, Counter, Errs}),
	    start(Counter - 1, Max, Opts, Errs1),
            ok;
        new_connection ->
            Step = proplists:get_value(report_step, Opts),
            case Counter rem Step of
                0 ->
                    logger:notice("Conns: ~p", [Counter]),
                    report({running, Counter, Errs});
                _ -> ok
            end,
            Me = self(),
            spawn_link(fun() -> start_client(Me, Opts) end),
            start(Counter + 1, Max, Opts, Errs)
    end.

start_client(Main, Opts) ->
    Host = proplists:get_value(host, Opts),
    Port = proplists:get_value(port, Opts),
    MsgInt = proplists:get_value(msg_interval, Opts),
    case gen_tcp:connect(Host, Port, [{active, false}, {packet, 0}], 5000) of
        {ok, Sock} ->
            loop(Main, Sock, MsgInt);
        {error, E} ->
            logger:error("Connection failed: ~p", [E]),
            Main ! {failed, {conn,  E}}
    end.

loop(Main, Sock, MsgInt) ->
    case gen_tcp:send(Sock, "hej") of
        ok ->
            case gen_tcp:recv(Sock, 0, 5000) of
                {ok, _} ->
                    timer:sleep(MsgInt),
                    loop(Main, Sock, MsgInt);
                {error, E} ->
                    Main ! {stopped, {recv, E}},
                    logger:error("Recv failed: ~p", [E])
            end;
        {error, E} ->
            Main ! {stopped, {send, E}},
            logger:error("Send failed: ~p", [E])
    end.

report(Info) ->
    case global:whereis_name(monmon) of
        undefined ->
            ok;
        Mon ->
            gen_server:cast(Mon, {report, node(), Info})
    end.
