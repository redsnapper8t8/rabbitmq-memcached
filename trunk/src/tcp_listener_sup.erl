%%% -------------------------------------------------------------------
%%% Author  : Flier Lu
%%% Description :
%%%
%%% Created : 2009-11-22
%%% -------------------------------------------------------------------
-module(tcp_listener_sup).

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
    AcceptorSupName = server_util:sup_name(tcp_acceptor_sup, IPAddress, Port),
    
    {ok, {{one_for_all, 10, 10},
          [{tcp_acceptor_sup, {tcp_acceptor_sup, start_link,
                               [AcceptorSupName, AcceptCallback]},
            transient, infinity, supervisor, [tcp_acceptor_sup]},
           {tcp_listener, {tcp_listener, start_link,
                           [IPAddress, Port, SocketOpts,
                            ConcurrentAcceptorCount, AcceptorSupName,
                            OnStartup, OnShutdown]},
            transient, 100, worker, [tcp_listener]}]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

