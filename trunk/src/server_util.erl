%% Author: Flier Lu
%% Created: 2009-11-23
%% Description: TODO: Add description to server_util
-module(server_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([sup_name/3, current_time/0]).

%%
%% API Functions
%%
sup_name(Protocol, IPAddress, Port) ->
    list_to_atom(
        lists:flatten(
            io_lib:format("~p@~s:~p", [Protocol, inet_parse:ntoa(IPAddress), Port])
        )
    ).

current_time() ->
    {MS, S, _} = now(),
    MS * 1000000 + S.

%%
%% Local Functions
%%

