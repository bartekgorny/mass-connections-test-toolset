%%%-------------------------------------------------------------------
%% @doc dummyclient public API
%% @end
%%%-------------------------------------------------------------------

-module(dummyclient_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Opts = application:get_all_env(dummyclient),
    MonitorNode = proplists:get_value(monitor, Opts),
    true = net_kernel:connect_node(MonitorNode),
    ok = register_me(),
    spawn(dummy_launcher, start, []),
    dummyclient_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

register_me() ->
    register_me(5).

register_me(0) ->
    {error, no_monitor_proc};
register_me(I) ->
    case global:whereis_name(monmon) of
        undefined ->
            timer:sleep(100),
            register_me(I - 1);
        Proc ->
            ok = gen_server:call(Proc, {register, node()})
    end.
