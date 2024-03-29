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
-module(rabbit_memcached_server).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("rabbit_memcached.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([ 'Socket'/2, 'Header'/2, 'Body'/2 ]).

-record(state, {
        socket,
        protocol,                    
        addr,
        port,      
        data= <<>>,     % data received from client 
        header= <<>>,   % parsed header
        body= <<>>,     % parsed body
        body_len=0,
        type=get,
        reqid,
        args
    }).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, {tcp, Socket}) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, {tcp, Socket}});

set_socket(Pid, {udp, Socket, Data, ReqId}) when is_pid(Pid), is_port(Socket), is_binary(Data), is_integer(ReqId) ->
    rabbit_memcached_stats:increment([{ bytes_read, size(Data)+8 }]),
    
    gen_fsm:send_event(Pid, {packet_ready, {udp, Socket, Data, ReqId}}).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, 'Socket', #state{}}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket, data=Data} = StateData) ->
    rabbit_memcached_stats:increment([{ bytes_read, size(Bin) }]),
    
    ?MODULE:StateName(data, StateData#state{data= <<Data/binary, Bin/binary>>});

handle_info({tcp_closed, Socket}, _StateName, #state{socket=Socket, addr=Addr} = StateData) ->
    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _StateName, #state{socket=Socket, protocol=tcp}) ->
    (catch gen_tcp:close(Socket)),
    rabbit_memcached_stats:increment([{curr_connections, -1}]),
    ok;

terminate(_Reason, _StateName, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal states
%% --------------------------------------------------------------------

'Socket'({socket_ready, {tcp, Socket}}, State) when is_port(Socket) ->
    {ok, {IP, Port}} = inet:peername(Socket),
    set_opts(header, {Socket}),
    rabbit_memcached_stats:increment([{curr_connections, 1}, {total_connections, 1}]),
    {next_state, 'Header', State#state{socket=Socket, protocol=tcp, addr=IP, port=Port}, ?COMMAND_TIMEOUT};

'Socket'({socket_ready, {udp, Socket, Data, IP, Port, ReqId}}, State) when is_port(Socket), is_binary(Data) ->
    {next_state, 'Header', State#state{socket=Socket, protocol=udp, data=Data, addr=IP, port=Port, reqid=ReqId}, ?COMMAND_TIMEOUT}.

'Header'(data, #state{data=Data, header=Header} = State) ->
    {Progress, NewData, NewHeader} = extract_header(Data, Header),
    State2 = State#state{data=NewData, header=NewHeader},
    case Progress of
        incomplete ->
            {next_state, 'Header', State2, ?COMMAND_TIMEOUT};
        empty ->
            {next_state, 'Header', State2, ?COMMAND_TIMEOUT};
        done ->
            {Cmd, Args} = parse_header(NewHeader),
            case Cmd of
                quit ->
                    {stop, normal, State2};
                _ ->
                    {NewStatus, NewState} = process_command(Cmd, Args, State2),
                    {next_state, NewStatus, NewState, ?COMMAND_TIMEOUT}
            end
    end.

'Body'(data, #state{body_len=BodyLen, data=Data} = State) when size(Data) >= BodyLen+2 ->
    <<Body:BodyLen/binary, "\r\n", Rest/binary>> = Data,
    {NewStatus, NewState} = process_body(State#state{body=Body, data=Rest}),
    gen_fsm:send_event(self(), data),
    {next_state, NewStatus, NewState, ?COMMAND_TIMEOUT};
'Body'(_, State) ->
    {next_state, 'Body', State, ?COMMAND_TIMEOUT}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

set_opts(header, {Sock}) ->
    inet:setopts(Sock, [{active, true}, {packet, raw}, binary]);
set_opts(body, {Sock, _Len}) ->
    inet:setopts(Sock, [{active, true}, {packet, raw}, binary]).

% parse a complete header from raw data, we assume a header ends with \r\n (0d 0a)
extract_header(<<"\r\n", Rest/binary>>, <<>>) ->
    {empty, Rest, <<>>};
extract_header(<<"\r\n", Rest/binary>>, Header) ->
    {done, Rest, Header};
extract_header(<<>>, Header) ->
    {incomplete, <<>>, Header};
extract_header(<<B:8, Rest/binary>>, Header) ->
    Header2 = <<Header/binary, B:8>>,
    extract_header(Rest, Header2).

to_integer(Str) ->
    {Int, []} = string:to_integer(Str),
    Int.

to_storage([Key, Flags, Exptime, Bytes]) ->
    Flags1 = to_integer(Flags),
    Exptime1 = to_integer(Exptime),
    Bytes1 = to_integer(Bytes),
    #storage{key=list_to_binary(Key), flags=Flags1, exptime=Exptime1, bytes=Bytes1}.

to_deletion([Key, Time]) ->
    Time1 = to_integer(Time),
    #deletion{key=list_to_binary(Key), time=Time1}.

parse_header(Data) ->
    Line = binary_to_list(Data),
    [Cmd|T] = string:tokens(Line, " "),
    case string:to_upper(Cmd) of
        "SET" ->
            {set, {set, to_storage(T)}};
        "ADD" ->
            {set, {add, to_storage(T)}};
        "REPLACE" ->
            {set, {replace, to_storage(T)}};
        "GET" ->        
            L = lists:map(fun(E) -> list_to_binary(E) end, T),
            {get, L};
        "DELETE" ->
            {delete, {to_deletion(T)}};
        "STATS" ->
            {stats, {}};
        "QUIT" ->
            {quit, {}};
        Other ->
            {unknown, Other}
    end.

construct_delete_result(deleted) ->
    <<"DELETED\r\n">>;
construct_delete_result(not_found) ->
    <<"NOT_FOUND\r\n">>.

construct_set_result(stored) ->
    <<"STORED\r\n">>;
construct_set_result(not_stored) ->
    <<"NOT_STORED\r\n">>.

construct_values(Values) ->
    Data = lists:foldl(fun(Entry, Acc) ->
            Bin = construct_entry(Entry),
            <<Acc/binary, Bin/binary>>
        end, <<>>, Values),
    <<Data/binary, "END\r\n">>.

construct_entry({_Queue, _Key, undefined}) ->
    <<>>;
construct_entry({Queue, _Key, Content}) ->
    DataSize = size(Content),
    Header = io_lib:format("VALUE ~s ~w ~w\r\n", [Queue, 0, DataSize]),    
    HeaderBin = case DataSize of
                    0 -> <<>>;
                    _ -> list_to_binary(Header)
                end,
    BodyBin = Content,
    <<HeaderBin/binary, BodyBin/binary, "\r\n">>.

process_queue_get(Queues) ->
    process_queue_get(Queues, []).
process_queue_get([], Values) ->    
    { ok, Values };
process_queue_get([Queue|L], Values) ->   
    case rabbit_memcached_worker:get(Queue) of
        {ok, {Key, Content}} ->
            rabbit_memcached_stats:increment([{ get_hits, 1 }]),
            
            process_queue_get(L, [{Queue, Key, Content}|Values]);
        empty ->
            rabbit_memcached_stats:increment([{ get_misses, 1 }]),
            
            process_queue_get(lists:filter(fun(Name) -> Name =/= Queue end, L), Values);
        {error, Error} ->                        
            {error, Error}
    end.

send_response(#state{socket=Socket, protocol=tcp}=State, Response) when is_binary(Response) ->
    rabbit_memcached_stats:increment([{ bytes_written, size(Response) }]),

    gen_tcp:send(Socket, Response),
    
    set_opts(header, {Socket}),
    {'Header', State#state{header= <<>>, body_len=0}};

send_response(#state{socket=Socket, protocol=udp, addr=IP, port=Port, reqid=ReqId}=State, Response) when is_binary(Response) ->
    Packet = <<ReqId:16, 0:16, 1:16, 0:16, Response/binary>>,

    rabbit_memcached_stats:increment([{ bytes_written, size(Packet) }]),

    gen_udp:send(Socket, IP, Port, Packet),
    {stop, normal, State}.

% GET
process_command(get, Keys, State) ->
    rabbit_memcached_stats:increment([{ cmd_get, 1 }]),    
    
    Response = case process_queue_get(Keys) of
        { ok, Values }->
            construct_values(Values);        
        { error, _Error } ->
            <<"ERROR\r\n">>
    end,
            
    send_response(State, Response);

% SET
process_command(set, {Method, Storage}, #state{socket=Socket, protocol=Protocol} = State) ->
    rabbit_memcached_stats:increment([{ cmd_set, 1 }]),
    
    % resume body receiving
    BodyLen = Storage#storage.bytes,
    
    case Protocol of
        tcp ->
            set_opts(body, {Socket, BodyLen})
    end,
            
    gen_fsm:send_event(self(), data),
    
    {'Body', State#state{header= <<>>, type=Method, args=Storage, body_len=BodyLen, body= <<>>}};

% DELETE
process_command(delete, {_Del}, #state{socket=Socket}=State) ->
    construct_delete_result(deleted),
    set_opts(header, {Socket}),
    {'Header', State#state{header= <<>>, body_len=0}};

% STATS
process_command(stats, {}, State) ->    
    Data = lists:foldl(
             fun({Name, Value}, Acc) ->
                Bin = iolist_to_binary(io_lib:format("STAT ~p ~s\r\n", [Name, Value])), 
                <<Acc/binary, Bin/binary>>
             end, <<>>, rabbit_memcached_stats:stats()),
    
    send_response(State, <<Data/binary, "END\r\n">>);

% Other
process_command(unknown, _Command, State) ->
    send_response(State, <<"ERROR\r\n">>).

process_exchange_set(Exchange, Msg) ->    
    case rabbit_memcached_worker:put(Exchange, Msg) of
        ok -> 
            construct_set_result(stored);
        {error, _Error} ->
            list_to_binary("ERROR\r\n")
    end.

process_body(#state{args=Storage, body=Body, type=Method} = State) ->
    Response = case Method of
        set ->
            process_exchange_set(Storage#storage.key, Body);
        _ ->
            list_to_binary("ERROR\r\n")
    end,
    
    send_response(State, Response).
