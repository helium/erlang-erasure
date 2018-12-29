-module(encode_decode_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_encode_decode_match/0, prop_encode_decode_match_gc/0]).

prop_encode_decode_match() ->
    ?FORALL(Players, gen_players(),
            begin
                F = (Players - 1) div 3,
                Threshold = F,
                K = 2*F,
                M = Players - K,
                Bin = crypto:strong_rand_bytes(Players*4),
                {ok, Shards} = erasure:encode(K, M, Bin),
                {ok, DecodedBin} = erasure:decode(K, M, Shards),
                ?WHENFAIL(begin
                              io:format("Players: ~p, Threshold: ~p, K: ~p, M: ~p~n", [Players, Threshold, K, M]),
                              io:format("Bin: ~p~n", [Bin]),
                              io:format("Shards: ~p~n", [Shards]),
                              io:format("Decoded: ~p~n", [DecodedBin])
                          end,
                          conjunction([
                                       {encode_decode_equality, eqc:equals(Bin, DecodedBin)}
                                      ]))
            end).

prop_encode_decode_match_gc() ->
    ?FORALL(Players, gen_players(),
            begin
                F = (Players - 1) div 3,
                Threshold = F,
                K = 2*F,
                M = Players - K,
                Bin = crypto:strong_rand_bytes(Players*4),
                {ok, Shards} = erasure:encode_gc(K, M, Bin),
                {ok, DecodedBin} = erasure:decode_gc(K, M, Shards),
                ?WHENFAIL(begin
                              io:format("Players: ~p, Threshold: ~p, K: ~p, M: ~p~n", [Players, Threshold, K, M]),
                              io:format("Bin: ~p~n", [Bin]),
                              io:format("Shards: ~p~n", [Shards]),
                              io:format("Decoded: ~p~n", [DecodedBin])
                          end,
                          conjunction([
                                       {encode_decode_equality, eqc:equals(Bin, DecodedBin)}
                                      ]))
            end).


gen_players() ->
    ?SUCHTHAT(A, int(), A > 4 andalso A < 70).
