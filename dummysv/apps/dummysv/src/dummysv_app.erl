%%%-------------------------------------------------------------------
%% @doc dummysrv public API
%% @end
%%%-------------------------------------------------------------------

-module(dummysv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    counter:start_link(),
    Opts = application:get_all_env(dummysv),
    MonitorNode = proplists:get_value(monitor, Opts),
    true = net_kernel:connect_node(MonitorNode),
    mlistener:start_link(7222, xxx, [], [], 7222, []).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
