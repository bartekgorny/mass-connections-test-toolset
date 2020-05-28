%%%-------------------------------------------------------------------
%%% @author bartlomiejgorny
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. May 2020 17:34
%%%-------------------------------------------------------------------
-module(monmon).
-author("bartlomiejgorny").
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([print/0, reset/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {nodes = #{}, server_state = #{}}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({global, monmon}, ?MODULE, [], []).

prt(X) ->
    io:format("~p~n", [X]).

print() ->
    gen_server:call(global:whereis_name(monmon), print).

reset() ->
    gen_server:call(global:whereis_name(monmon), reset).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #state{nodes = #{}, server_state = #{}}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
                     {reply, Reply :: term(), NewState :: #state{}} |
                     {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                     {stop, Reason :: term(), NewState :: #state{}}).
handle_call(reset, _From, State) ->
    case global:whereis_name(server_counter) of 
        undefined -> ok;
        Srv -> gen_server:cast(Srv, reset)
    end,
    {reply, ok, State#state{nodes = #{}}};
handle_call({register, Node}, _From, State) ->
    monitor_node(Node, true),
    {reply, ok, nodestate(Node, started, State)};
handle_call(print, _From, State) ->
    io:format("~n"),
    io:format("==============================~n"),
    lists:map(fun({N, S}) -> io:format("~p : ~p~n", [N, S]) end, maps:to_list(State#state.nodes)),
    Total = sum_connections(State#state.nodes),
    io:format("------------------------------~n"),
    io:format("Total: ~p~n", [Total]),
    io:format("Server state: ~p~n", [State#state.server_state]),
    io:format("~n"),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({report, Node, Info}, State) ->
    {noreply, nodestate(Node, Info, State)};
handle_cast({server_state, Info}, State) ->
    {noreply, State#state{server_state = Info}};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({nodedown, Node}, State) ->
    {noreply, nodestate(Node, down, State)};
handle_info(Info, State) ->
    prt(Info),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
                     {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

nodestate(Node, Info, State) ->
    State#state{nodes = maps:put(Node, Info, State#state.nodes)}.

sum_connections(Nodes) ->
    lists:sum(lists:map(fun extract_count/1, maps:values(Nodes))).

extract_count({running, X, _}) -> X;
extract_count({over, _, X}) -> X;
extract_count(_) -> 0.

