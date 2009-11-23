%% Author: Flier Lu
%% Created: 2009-11-22
%% Description: TODO: Add description to rabbit_memcached
-module(rabbit_memcached_app).

-behaviour(application).
-behaviour(supervisor).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([ start/2, stop/1, init/1 ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([ start/0, 
          stop/0,
          listener_started/2,
          listener_stopped/2,
          start_server/2
        ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------
start() -> 
    start(normal, []),
    ok.

stop() -> 
    ok.

%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(normal, []) -> 
    supervisor:start_link( { local, rabbit_memcached_sup }, ?MODULE, []).

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, ServerModule} = application:get_env(server_module),       
    SupFlags = {one_for_one, 5, 3600},
    ServerSup = childspec({server, ServerModule}),
    StatsSpec = childspec(memcached_stats),
    WorkerSpec = childspec(memcached_worker),
    {ok, Listeners} = application:get_env(listeners),
    ListenerSups = lists:map(fun({Protocol, Addr, Port}) -> 
                                childspec({listener, Protocol, Addr, Port, ServerModule}) 
                            end, Listeners),    
    {ok, {SupFlags, [ServerSup, StatsSpec, WorkerSpec] ++ ListenerSups}}.    
    
%% ====================================================================
%% Internal functions
%% ====================================================================
listener_started(_IPAddress, _Port) -> ok.

listener_stopped(_IPAddress, _Port) -> ok.

start_server(ServerModule, Sock) ->
    io:format("start server ~p ~p\n", [ServerModule, Sock]),
    {ok, Child} = supervisor:start_child(rabbit_memcached_server_sup, []),
    ok = gen_tcp:controlling_process(Sock, Child),
    ServerModule:set_socket(Child, Sock),
    Child.

childspec({server, Module}) ->    
    {
        rabbit_memcached_server_sup,
        {
            rabbit_memcached_server_sup, 
            start_link, 
            [{local, rabbit_memcached_server_sup}, {Module, start_link, []}]
        },
        permanent,
        infinity,
        supervisor,
        [rabbit_memcached_server_sup]
    };

childspec(memcached_stats) ->
    {
        rabbit_memcached_stats,
        { rabbit_memcached_stats, start_link, []},
        permanent, 2000, worker, []
    };

childspec(memcached_worker) ->
    { ok, Params } = application:get_env(server_mode),
    {
        rabbit_memcached_worker,
        { rabbit_memcached_worker, start_link, [Params]},
        permanent, 2000, worker, []     
    };

childspec({listener, Protocol, Host, Port, ServerModule}) ->
    IPAddress =
        case inet:getaddr(Host, inet) of
            {ok, IPAddress1} -> IPAddress1;
            {error, Reason} ->
                error_logger:error_msg("invalid host ~p - ~p~n", [Host, Reason]),
                throw({error, {invalid_host, Host, Reason}})
        end,
    
    if is_integer(Port) andalso (Port >= 0) andalso (Port =< 65535) 
            -> ok;
       true 
            -> error_logger:error_msg("invalid port ~p - not 0..65535~n", [Port]),
               throw({error, invalid_port, Port})
    end,
    
    Name = server_util:sup_name(Protocol, IPAddress, Port),
    SupName = 
        case Protocol of
            tcp -> tcp_listener_sup;
            udp -> udp_listener_sup
        end,    
    
    {
        Name,
        {
            SupName,
            start_link,
            [
                IPAddress, Port,
                [binary, {packet, raw}, {exit_on_close, false}],
                {?MODULE, listener_started, []},
                {?MODULE, listener_stopped, []},
                {?MODULE, start_server, [ServerModule]}
            ]
        },
        transient,
        infinity,
        supervisor,
        [SupName]
    }.
