%%==============================================================================
%% Copyright 2018 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(mlistener).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(supervisor).

%% We do not block on send anymore.
-define(TCP_SEND_TIMEOUT, 15000).

-export([start_link/6, init/1]).

%% Internal
-export([start_accept_loop/3, accept_loop/3]).

-type connection_details() :: #{
        proxy        := boolean(),
        version      => 1 | 2,
        src_address  := inet:ip_address() | binary(),
        src_port     := inet:port_number(),
        dest_address := inet:ip_address() | binary(),
        dest_port    := inet:port_number()
       }.
-export_type([connection_details/0]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start_link(PortIP, Module, Opts, SockOpts, Port, IPS) ->
    supervisor:start_link(?MODULE, {PortIP, Module, Opts, SockOpts, Port, IPS}).

init({PortIP, Module, Opts, SockOpts, Port, IPS}) ->
    try
        AcceptorsNum = proplists:get_value(acceptors_num, Opts, 10),
        ListenSocket = listen_tcp(PortIP, Module, SockOpts, Port, IPS),
        Children = [make_childspec({PortIP, I}, ListenSocket, Module, Opts)
                    || I <- lists:seq(1, AcceptorsNum)],
        {ok, {#{strategy => one_for_one, intensity => 100, period => 1}, Children}}
    catch
        Error -> exit(Error)
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

prt(X) ->
    io:format("~p~n", [X]).


start_accept_loop(ListenSock, Module, Opts) ->
    Pid = proc_lib:spawn_link(?MODULE, accept_loop, [ListenSock, Module, Opts]),
    {ok, Pid}.

accept_loop(ListenSocket, Module, Opts) ->
    case do_accept(ListenSocket) of
        {ok, Socket, ConnectionDetails} ->
            msocket:start(
              Module, gen_tcp, Socket, [{connection_details, ConnectionDetails} | Opts]),
            ?MODULE:accept_loop(ListenSocket, Module, Opts);
        {error, E} ->
            counter:err({accept, E}),
            logger:error("Accept failed: ~p", [E]),
            ?MODULE:accept_loop(ListenSocket, Module, Opts)
    end.

do_accept(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            {ok, {DestAddr, DestPort}} = inet:sockname(Socket),
            {ok, {SrcAddr, SrcPort}} = inet:peername(Socket),
            {ok, Socket, #{proxy => false,
                           src_address => SrcAddr,
                           src_port => SrcPort,
                           dest_address => DestAddr,
                           dest_port => DestPort}};
        Other ->
            Other
    end.

-spec make_childspec(Id :: term(), ListenSock :: port(),
                     Module :: module(), Opts :: [any()]) ->
                            supervisor:child_spec().
make_childspec(Id, ListenSock, Module, Opts) ->
    #{id => Id,
      start => {?MODULE, start_accept_loop, [ListenSock, Module, Opts]},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => [?MODULE]}.

-spec listen_tcp(PortIPPRoto :: ejabberd_listener:port_ip_proto(),
                 Module :: atom(),
                 SockOpts :: [gen_tcp:listen_option()],
                 Port :: inet:port_number(),
                 IPS :: [any()]) -> port().
listen_tcp(_PortIPProto, _Module, SockOpts, Port, _IPS) ->
    DefaultSockOpts = [binary,
                       {backlog, 100},
                       {packet, 0},
                       {active, false},
                       {reuseaddr, true},
                       {nodelay, true},
                       {send_timeout, ?TCP_SEND_TIMEOUT},
                       {keepalive, true},
                       {send_timeout_close, true}],
    FinalSockOpts = override_sock_opts(SockOpts, DefaultSockOpts),
    Res = listen_or_retry(Port, FinalSockOpts, 10),
    case Res of
        {ok, ListenSocket} ->
            ListenSocket;
        {error, Reason} ->
            {error, Reason}
    end.

%% Process exit and socket release are not transactional
%% So, there can be a short period of time when we can't bind
listen_or_retry(Port, SockOpts, Retries) ->
    case gen_tcp:listen(Port, SockOpts) of
        {ok, ListenSocket} ->
            {ok, ListenSocket};
        {error, eaddrinuse} when Retries > 0 ->
            timer:sleep(100),
            listen_or_retry(Port, SockOpts, Retries-1);
        {error, Reason} ->
            {error, Reason}
    end.

override_sock_opts([], Opts) ->
    Opts;
override_sock_opts([Override | OverrideOpts], Opts) ->
    NewOpts = do_override(Override, Opts),
    override_sock_opts(OverrideOpts, NewOpts).

do_override({ip, _} = IP, Opts) ->
    lists:keystore(ip, 1, Opts, IP);
do_override({backlog, _} = Backlog, Opts) ->
    lists:keystore(backlog, 1, Opts, Backlog);
do_override(inet6, Opts) ->
    [inet6 | lists:delete(inet6, Opts)];
do_override(inet, Opts) ->
    [inet | lists:delete(inet, Opts)];
do_override(_, Opts) ->
    Opts.
