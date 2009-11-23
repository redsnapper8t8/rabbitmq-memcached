%%% -------------------------------------------------------------------
%%% Author  : Flier Lu
%%% Description :
%%%
%%% Created : 2009-11-21
%%% -------------------------------------------------------------------
-module(rabbit_memcached_stats).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("rabbit_memcached.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0]).
-export([stats/0, increment/1, increment/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    cmd_get = 0,
    cmd_set = 0,
    get_hits = 0,
    get_misses = 0,
    bytes_read = 0,
    bytes_written = 0,
    curr_connections = 0,
    total_connections = 0,
    uptime = server_util:current_time()                
    }).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

increment(Items) ->
    gen_server:call(?MODULE, {increment, Items}).

increment(Name, Incr) ->
    increment([{Name, Incr}]).

stats() ->
    gen_server:call(?MODULE, {stats}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({increment, Items}, _From, State) ->    
    {reply, ok, do_increment(Items, State)};

handle_call({stats}, _From, State) ->    
    {reply, do_stats(State), State};

handle_call(Request, _From, State) ->
    {reply, {unknown_call, Request}, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
do_increment([], State) ->
    State;
do_increment([{ cmd_get, Value } | L], State = #state{ cmd_get = Cur }) when is_integer(Value) ->
    do_increment(L, State#state{ cmd_get = Cur + Value });
do_increment([{ cmd_set, Value } | L], State = #state{ cmd_set = Cur }) when is_integer(Value) ->
    do_increment(L, State#state{ cmd_set = Cur + Value });
do_increment([{ get_hits, Value } | L], State = #state{ get_hits = Cur }) when is_integer(Value) ->
    do_increment(L, State#state{ get_hits = Cur + Value });
do_increment([{ get_misses, Value } | L], State = #state{ get_misses = Cur }) when is_integer(Value) ->
    do_increment(L, State#state{ get_misses = Cur + Value });
do_increment([{ bytes_read, Value } | L], State = #state{ bytes_read = Cur }) when is_integer(Value) ->
    do_increment(L, State#state{ bytes_read = Cur + Value });
do_increment([{ bytes_written, Value } | L], State = #state{ bytes_written = Cur }) when is_integer(Value) ->
    do_increment(L, State#state{ bytes_written = Cur + Value });
do_increment([{ curr_connections, Value } | L], State = #state{ curr_connections = Cur }) when is_integer(Value) ->
    do_increment(L, State#state{ curr_connections = Cur + Value });
do_increment([{ total_connections, Value } | L], State = #state{ total_connections = Cur }) when is_integer(Value) ->
    do_increment(L, State#state{ total_connections = Cur + Value }).

do_stats(State) ->
    [
        { cmd_get, integer_to_list(State#state.cmd_get) },
        { cmd_set, integer_to_list(State#state.cmd_set) },
        { get_hits, integer_to_list(State#state.get_hits) },
        { get_misses, integer_to_list(State#state.get_misses) },
        { bytes_read, integer_to_list(State#state.bytes_read) },
        { bytes_written, integer_to_list(State#state.bytes_written) },
        { curr_connections, integer_to_list(State#state.curr_connections) },
        { total_connections, integer_to_list(State#state.total_connections) },
        { uptime, integer_to_list(State#state.uptime) },
        
        { pid, os:getpid() },
        { time, integer_to_list(server_util:current_time()) },
        { version, ?VERSION}        
    ].
