-module(encode_decode_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_encode_decode_match/0]).

prop_encode_decode_match() ->
    ?FORALL({Players, Threshold}, gen_players_threshold(),
            begin
                K = 2*Threshold,
                M = Players - K,
                Bin = crypto:hash(sha256, crypto:strong_rand_bytes(128)),
                Encoded = erasure:encode(K, M, Bin),
                {ok, DecodedBin} = erasure:decode(K, M, Encoded),
                ?WHENFAIL(begin
                              io:format("Players: ~p, Threshold: ~p, K: ~p, M: ~p~n", [Players, Threshold, K, M]),
                              io:format("Bin: ~p", [Bin]),
                              io:format("Encoded: ~p", [Encoded]),
                              io:format("Decoded: ~p", [DecodedBin])
                          end,
                          conjunction([
                                       {encode_decode_equality, eqc:equals(Bin, DecodedBin)}
                                      ]))
            end).

gen_players_threshold() ->
    ?SUCHTHAT({Players, Threshold},
              ?LET({X, Y},
                   ?SUCHTHAT({A, B}, {int(), int()}, A > 0 andalso B >= 0 andalso A > B),
                   {X*3, X - Y}),
              Players > 3*Threshold+1 andalso Threshold > 1).
