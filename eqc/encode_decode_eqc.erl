-module(encode_decode_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_encode_decode_match/0, prop_encode_decode_match_gc/0]).

prop_encode_decode_match() ->
    ?FORALL({Players, Bin, RandomShardIndices}, gen_random_shards(),
            begin
                F = ceil((Players - 1) / 3),
                Threshold = F,
                K = 2*F,
                M = Players - K,
                %Bin = crypto:strong_rand_bytes(Players*4),
                {ok, Shards} = erasure:encode(K, M, Bin),
                RandomShards = [lists:nth(I, Shards) || I <- lists:usort(RandomShardIndices)],
                {ok, DecodedBin} = erasure:decode(K, M, Shards),
                {ok, RandomDecodedBin} = RandomDecodeResult = erasure:decode(K, M, RandomShards),
                ShouldHaveDecoded = length(lists:usort(RandomShardIndices)) >= K,
                %io:format("Random shards would be ~w -- ~p~n", [RandomShardIndices, ShouldHaveDecoded]),
                ?WHENFAIL(begin
                              io:format("Players: ~p, Threshold: ~p, K: ~p, M: ~p~n", [Players, Threshold, K, M]),
                              io:format("Bin: ~p~n", [Bin]),
                              io:format("Shards: ~p~n", [Shards]),
                              io:format("Random Shards ~w~n", [RandomShardIndices]),
                              io:format("Decoded: ~p~n", [DecodedBin]),
                              io:format("RandomDecoded: ~p~n", [RandomDecodedBin])
                          end,
                          conjunction([
                                       {encode_decode_equality, eqc:equals(Bin, DecodedBin)},
                                       {random_encode_decode_equality, eqc:equals(Bin, RandomDecodedBin)},
                                       {random_shards_decoded, eqc:equals(ShouldHaveDecoded, element(1, RandomDecodeResult) == ok)}
                                      ]))
            end).


prop_encode_decode_match_gc() ->
    ?FORALL({Players, Bin, RandomShardIndices}, gen_random_shards(),
            begin
                F = ceil((Players - 1) / 3),
                Threshold = F,
                K = 2*F,
                M = Players - K,
                W = ceil(math:log2(K + M)),
                %Bin = crypto:strong_rand_bytes(Players*4),
                {ok, Shards} = erasure:encode_gc(K, M, W, Bin),
                RandomShards = [lists:nth(I, Shards) || I <- lists:usort(RandomShardIndices)],
                {ok, DecodedBin} = erasure:decode_gc(K, M, W, Shards),
                {ok, RandomDecodedBin} = RandomDecodeResult = erasure:decode_gc(K, M, W, RandomShards),
                ShouldHaveDecoded = length(lists:usort(RandomShardIndices)) >= K,
                %io:format("Random shards would be ~w -- ~p~n", [RandomShardIndices, ShouldHaveDecoded]),
                ?WHENFAIL(begin
                              io:format("Players: ~p, Threshold: ~p, K: ~p, M: ~p~n", [Players, Threshold, K, M]),
                              io:format("Bin: ~p~n", [Bin]),
                              io:format("Shards: ~p~n", [Shards]),
                              io:format("Random Shards ~w~n", [RandomShardIndices]),
                              io:format("Decoded: ~p~n", [DecodedBin]),
                              io:format("RandomDecoded: ~p~n", [RandomDecodedBin])
                          end,
                          conjunction([
                                       {encode_decode_equality, eqc:equals(Bin, DecodedBin)},
                                       {random_encode_decode_equality, eqc:equals(Bin, RandomDecodedBin)},
                                       {random_shards_decoded, eqc:equals(ShouldHaveDecoded, element(1, RandomDecodeResult) == ok)}
                                      ]))
            end).


gen_players() ->
    ?SUCHTHAT(A, int(), A > 4 andalso A < 70).

gen_random_shards() ->
    ?SUCHTHAT({N, _, L}, ?LET(N, gen_players(),
                              {N, eqc_gen:binary(N*16), eqc_gen:list(eqc_gen:elements(lists:seq(1, N)))}),
              length(lists:usort(L)) >= 2 * ceil((N - 1) / 3)).
