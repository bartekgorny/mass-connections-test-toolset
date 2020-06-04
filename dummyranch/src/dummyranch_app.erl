%%%-------------------------------------------------------------------
%% @doc dummyranch public API
%% @end
%%%-------------------------------------------------------------------

-module(dummyranch_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    counter:start_link(),
    Opts = application:get_all_env(dummyranch),
    MonitorNode = proplists:get_value(monitor, Opts),
    Proto = proplists:get_value(protocol, Opts),
    true = net_kernel:connect_node(MonitorNode),
    ranch:start_listener(tcp_echo, ranch_tcp, [{port, 7222}], Proto, []),
    dummyranch_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
