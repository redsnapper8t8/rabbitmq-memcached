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
-module(rabbit_memcached_udp_listener_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/6]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([init/1]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start_link(IPAddress, Port, SocketOpts, OnStartup, OnShutdown, Callback) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, 
                          {IPAddress, Port, SocketOpts, OnStartup, OnShutdown, Callback}).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init({IPAddress, Port, SocketOpts, OnStartup, OnShutdown, Callback}) ->    
    {ok, {{one_for_all, 10, 10},
          [{rabbit_memcached_udp_listener, {rabbit_memcached_udp_listener, start_link,
            [IPAddress, Port, SocketOpts, OnStartup, OnShutdown, Callback]},
            transient, 100, worker, [rabbit_memcached_udp_listener]}]}}.
    
%% ====================================================================
%% Internal functions
%% ====================================================================

