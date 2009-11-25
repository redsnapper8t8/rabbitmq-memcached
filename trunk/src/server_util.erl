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

