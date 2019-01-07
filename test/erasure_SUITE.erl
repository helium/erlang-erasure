-module(erasure_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
         simple_test/1,
         cauchy_test/1,
         cauchy_higher_w_test/1
        ]).

all() ->
    [
     simple_test,
     cauchy_test,
     cauchy_higher_w_test
    ].


init_per_testcase(_, Config) ->
    K = 7,
    M = 3,
    W = ceil(math:log2(K+M)),
    Data = <<"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Morbi nec nisi interdum, ultricies mauris eget, congue ante. Fusce erat diam, lacinia eu volutpat ut, gravida quis justo. Maecenas sagittis, ligula.">>,
    [ {k, K}, {m, M}, {w, W}, {data, Data} | Config].

end_per_testcase(_, _Config) ->
    ok.

simple_test(Config) ->
    K = proplists:get_value(k, Config),
    M = proplists:get_value(m, Config),
    Data = proplists:get_value(data, Config),
    {ok, Shards} = erasure:encode(K, M, Data),
    ?assertEqual({ok, Data}, erasure:decode(K, M, Shards)),
    ?assertEqual({ok, Data}, erasure:decode(K, M, lists:sublist(Shards, K))),
    ?assertEqual({ok, Data}, erasure:decode(K, M, lists:reverse(lists:sublist(Shards, K)))),
    ?assertMatch({error, _}, erasure:decode(K, M, lists:sublist(Shards, K - 1))),
    ?assertMatch({error, _}, erasure:decode(K, M, lists:sublist(Shards, K - 1) ++ [hd(Shards)])),
    ok.

cauchy_test(Config) ->
    K = proplists:get_value(k, Config),
    M = proplists:get_value(m, Config),
    W = proplists:get_value(w, Config),
    Data = proplists:get_value(data, Config),
    {ok, Shards} = erasure:encode_gc(K, M, W, Data),
    {ok, Shards2} = erasure:encode_gc(K, M, Data),
    ?assertEqual(Shards2, Shards),
    ?assertEqual({ok, Data}, erasure:decode_gc(K, M, Shards)),
    ?assertEqual({ok, Data}, erasure:decode_gc(K, M, lists:sublist(Shards, K))),
    ?assertEqual({ok, Data}, erasure:decode_gc(K, M, lists:reverse(lists:sublist(Shards, K)))),
    ?assertMatch({error, _}, erasure:decode_gc(K, M, lists:sublist(Shards, K - 1))),
    ?assertMatch({error, _}, erasure:decode_gc(K, M, lists:sublist(Shards, K - 1) ++ [hd(Shards)])),
    ?assertEqual({ok, Data}, erasure:decode_gc(K, M, W, Shards)),
    ?assertEqual({ok, Data}, erasure:decode_gc(K, M, W, lists:sublist(Shards, K))),
    ?assertEqual({ok, Data}, erasure:decode_gc(K, M, W, lists:reverse(lists:sublist(Shards, K)))),
    ?assertMatch({error, _}, erasure:decode_gc(K, M, W, lists:sublist(Shards, K - 1))),
    ?assertMatch({error, _}, erasure:decode_gc(K, M, W, lists:sublist(Shards, K - 1) ++ [hd(Shards)])),
    ok.

cauchy_higher_w_test(Config) ->
    K = proplists:get_value(k, Config),
    M = proplists:get_value(m, Config),
    W = proplists:get_value(w, Config) + 5,
    Data = proplists:get_value(data, Config),
    {ok, Shards} = erasure:encode_gc(K, M, W, Data),
    ?assertEqual({ok, Data}, erasure:decode_gc(K, M, W, Shards)),
    ?assertEqual({ok, Data}, erasure:decode_gc(K, M, W, lists:sublist(Shards, K))),
    ?assertEqual({ok, Data}, erasure:decode_gc(K, M, W, lists:reverse(lists:sublist(Shards, K)))),
    ?assertMatch({error, _}, erasure:decode_gc(K, M, W, lists:sublist(Shards, K - 1))),
    ?assertMatch({error, _}, erasure:decode_gc(K, M, W, lists:sublist(Shards, K - 1) ++ [hd(Shards)])),
    ok.
