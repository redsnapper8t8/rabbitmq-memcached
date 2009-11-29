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
-module(rabbit_memcached_tcp_listener_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/6, start_link/7]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 init/1
        ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start_link(IPAddress, Port, SocketOpts, OnStartup, OnShutdown, AcceptCallback) ->
    start_link(IPAddress, Port, SocketOpts, OnStartup, OnShutdown, AcceptCallback, 1).

start_link(IPAddress, Port, SocketOpts, OnStartup, OnShutdown,
           AcceptCallback, ConcurrentAcceptorCount) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {IPAddress, Port, SocketOpts, 
        OnStartup, OnShutdown, AcceptCallback, ConcurrentAcceptorCount}).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init({IPAddress, Port, SocketOpts, OnStartup, OnShutdown,
      AcceptCallback, ConcurrentAcceptorCount}) ->    
    AcceptorSupName = rabbit_memcached_server_util:sup_name(tcp_acceptor_sup, IPAddress, Port),
    
    {ok, {{one_for_all, 10, 10},
          [{rabbit_memcached_tcp_acceptor_sup, {rabbit_memcached_tcp_acceptor_sup, start_link,
                               [AcceptorSupName, AcceptCallback]},
            transient, infinity, supervisor, [rabbit_memcached_tcp_acceptor_sup]},
           {rabbit_memcached_tcp_listener, {rabbit_memcached_tcp_listener, start_link,
                           [IPAddress, Port, SocketOpts,
                            ConcurrentAcceptorCount, AcceptorSupName,
                            OnStartup, OnShutdown]},
            transient, 100, worker, [rabbit_memcached_tcp_listener]}]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

