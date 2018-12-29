-module(erasure_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
         simple_test/1,
         cauchy_test/1
        ]).

all() ->
    [
     simple_test,
     cauchy_test
    ].


init_per_testcase(_, Config) ->
    K = 7,
    M = 3,
    Data = <<"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Morbi nec nisi interdum, ultricies mauris eget, congue ante. Fusce erat diam, lacinia eu volutpat ut, gravida quis justo. Maecenas sagittis, ligula.">>,
    [ {k, K}, {m, M}, {data, Data} | Config].

end_per_testcase(_, _Config) ->
    ok.

simple_test(Config) ->
    K = proplists:get_value(k, Config),
    M = proplists:get_value(m, Config),
    Data = proplists:get_value(data, Config),
    {ok, Shards} = erasure:encode(K, M, Data),
    ct:pal("Shards: ~p", [Shards]),
    ?assertEqual({ok, Data}, erasure:decode(K, M, Shards)),
    ?assertEqual({ok, Data}, erasure:decode(K, M, lists:sublist(Shards, K))),
    ?assertEqual({ok, Data}, erasure:decode(K, M, lists:reverse(lists:sublist(Shards, K)))),
    ?assertMatch({error, _}, erasure:decode(K, M, lists:sublist(Shards, K - 1))),
    ?assertMatch({error, _}, erasure:decode(K, M, lists:sublist(Shards, K - 1) ++ [hd(Shards)])),
    ok.

cauchy_test(Config) ->
    K = proplists:get_value(k, Config),
    M = proplists:get_value(m, Config),
    Data = proplists:get_value(data, Config),
    {ok, Shards} = erasure:encode_gc(K, M, Data),
    ct:pal("Shards: ~p", [Shards]),
    ?assertEqual({ok, Data}, erasure:decode_gc(K, M, Shards)),
    ?assertEqual({ok, Data}, erasure:decode_gc(K, M, lists:sublist(Shards, K))),
    ?assertEqual({ok, Data}, erasure:decode_gc(K, M, lists:reverse(lists:sublist(Shards, K)))),
    ?assertMatch({error, _}, erasure:decode_gc(K, M, lists:sublist(Shards, K - 1))),
    ?assertMatch({error, _}, erasure:decode_gc(K, M, lists:sublist(Shards, K - 1) ++ [hd(Shards)])),
    ok.
