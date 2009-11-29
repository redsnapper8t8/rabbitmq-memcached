%% Author: Flier Lu
%% Created: 2009-11-29
%% Description: TODO: Add description to rabbit_memcached_tests
-module(rabbit_memcached_server_tests).

%%
%% Include files
%%
-include("rabbit_memcached.hrl").
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([]).
-import(rabbit_memcached_server, [extract_header/2, parse_header/1]).

%%
%% API Functions
%%

%%
%% Local Functions
%%

extract_header_test() ->
    ?assertMatch({empty, _, <<>>}, extract_header(<<"\r\n">>, <<>>)),
    ?assertMatch({done, <<"data">>, <<"header">>}, extract_header(<<"header\r\ndata">>, <<>>)),
    ?assertMatch({incomplete, <<>>, <<"header">>}, extract_header(<<"header">>, <<>>)),
    ok.

to_storage_test() ->
    ?assertMatch({set, {set, #storage{key= <<"key">>, flags=1, exptime=2, bytes=3}}}, parse_header(<<"set key 1 2 3">>)),
    ?assertMatch({set, {add, #storage{key= <<"key">>, flags=1, exptime=2, bytes=3}}}, parse_header(<<"add key 1 2 3">>)),
    ?assertMatch({set, {replace, #storage{key= <<"key">>, flags=1, exptime=2, bytes=3}}}, parse_header(<<"replace key 1 2 3">>)),
    ?assertMatch({get, [<<"key1">>, <<"key2">>, <<"key3">>]}, parse_header(<<"get key1 key2 key3">>)),
    ?assertMatch({stats, {}}, parse_header(<<"stats">>)),
    ?assertMatch({quit, {}}, parse_header(<<"quit">>)),
    ?assertMatch({unknown, "TEST"}, parse_header(<<"test">>)),
    ok.
