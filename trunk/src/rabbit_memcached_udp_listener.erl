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
-module(rabbit_memcached_udp_listener).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {callback, sock, on_startup, on_shutdown}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(IPAddress, Port, SocketOpts, OnStartup, OnShutdown, Callback) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
        {IPAddress, Port, SocketOpts, OnStartup, OnShutdown, Callback},
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
init({Addr, Port, SocketOpts,  {M, F, A} = OnStartup, OnShutdown, Callback}) ->
    process_flag(trap_exit, true),
    
    OpenOpts = [{ip, Addr}],
    
    try gen_udp:open(Port, SocketOpts ++ OpenOpts) of
        {ok, Socket} ->
            error_logger:info_msg("started memcached UDP listener on ~s:~p~n", 
                                  [inet_parse:ntoa(Addr), Port]),
            
            apply(M, F, A ++ [Addr, Port]),
            
            {ok, #state{callback=Callback, sock=Socket, 
                        on_startup=OnStartup, on_shutdown=OnShutdown}};
        {error, Reason} ->
            {stop, {cannot_open, Addr, Port, Reason}}
    catch
        _:Error ->
            io:format("fail to open UDP port ~p", [Error]),
            {stop, Error}
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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_info({udp, Socket, IP, InPortNo, Packet}, State = #state{callback={M, F, A}}) ->
    case Packet of
        <<ReqId:16, _SeqNo:16, _Count:16, _Reserved:16, Data/binary>> when _Reserved =:= 0 ->                           
            %% report
            {ok, {Address, Port}} = inet:sockname(Socket),
    
            error_logger:info_msg("received memcached UDP request on ~s:~p from ~s:~p~n",
                                  [inet_parse:ntoa(Address), Port,
                                   inet_parse:ntoa(IP), InPortNo]),
    
            %% handle
            apply(M, F, A ++ [{udp, Socket, Data, IP, InPortNo, ReqId}]);
        _ ->
            error_logger:info_msg("drop the invalid packet from ~p:~p with ~p bytes", 
                                  [inet_parse:ntoa(IP), InPortNo, size(Packet)])
    end,

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, #state{sock=Socket, on_shutdown={M, F, A}}) ->
    {ok, {Addr, Port}} = inet:sockname(Socket),
    
    gen_udp:close(Socket),
   
    error_logger:info_msg("stopped memcached UDP listener on ~s:~p~n",
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

