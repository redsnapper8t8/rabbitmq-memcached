%%   The contents of this file are subject to the Mozilla Public Licenses
%%   Version 1.1 (the "License"); you may not use this file except in
%%   compliance with the License. You may obtain a copy of the License at
%%   http://www.mozilla.org/MPL/
%%
%%   Software distributed under the License is distributed on an "AS IS"
%%   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%   License for the specific language governing rights and limitations
%%   under the License.
%%
%%
%%   Contributor(s): ______________________________________.
%%
-module(tcp_listener).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/7]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {sock, on_startup, on_shutdown}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Addr, Port, Opts, ConcurrentAcceptorCount, AcceptorSup, OnStartup, OnShutdown) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
        {Addr, Port, Opts, ConcurrentAcceptorCount, AcceptorSup, OnStartup, OnShutdown},
        []
    ).

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
init({Addr, Port, Opts, ConcurrentAcceptorCount, AcceptorSup, {M, F, A} = OnStartup, OnShutdown}) ->
    process_flag(trap_exit, true),
    ListenOpts = [  {ip, Addr},
                    {active, false},
                    {backlog, 30},
                    {keepalive, true},
                    {reuseaddr, true} ],
    case gen_tcp:listen(Port, Opts ++ ListenOpts) of
        {ok, LSock} ->
            lists:foreach(fun(_) ->
                            {ok, _APid} = supervisor:start_child(AcceptorSup, [LSock])
                          end, 
                          lists:duplicate(ConcurrentAcceptorCount, dummy)),
            
            error_logger:info_msg("started memcached TCP listener on ~s:~p~n", 
                                  [inet_parse:ntoa(Addr), Port]),
            
            apply(M, F, A ++ [Addr, Port]),
            
            {ok, #state{sock=LSock, on_startup=OnStartup, on_shutdown=OnShutdown}};
        {error, Reason} ->
            error_logger:error_msg( "failed to start memcached TCP listener on ~s:~p - ~p~n", 
                                    [inet_parse:ntoa(Addr), Port, Reason]),
            
            {stop, {cannot_listen, Addr, Port, Reason}}
    end.

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
handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

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
terminate(_Reason, #state{sock=LSock, on_shutdown={M, F, A}}) ->
    {ok, {Addr, Port}} = inet:sockname(LSock),
    gen_tcp:close(LSock),
   
    error_logger:info_msg("stopped memcached TCP listener on ~s:~p~n",
                          [inet_parse:ntoa(Addr), Port]),
    
    apply(M, F, A ++ [Addr, Port]).

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

