-module(erasure).

-export([encode/3,
         encode_gc/3, encode_gc/4,
         decode/3,
         decode_gc/3, decode_gc/4
        ]).

-on_load(init/0).

-define(APPNAME, erasure).
-define(LIBNAME, 'erasure').

-type shards() :: [{Index :: non_neg_integer(), TotalSize :: pos_integer(), binary()}, ...].

-spec encode(K :: pos_integer(), M :: pos_integer(), binary()) -> {ok, shards()}.
encode(_, _, _) ->
    not_loaded(?LINE).

-spec encode_gc(K :: pos_integer(), M :: pos_integer(), Bin :: binary()) -> {ok, shards()}.
encode_gc(K, M, Bin) ->
    encode_gc(K, M, ceil(math:log2(K+M)), Bin).

-spec encode_gc(K :: pos_integer(), M :: pos_integer(), W :: pos_integer(), binary()) -> {ok, shards()}.
encode_gc(_, _, _, _) ->
    not_loaded(?LINE).

-spec decode(K :: pos_integer(), M :: pos_integer(), Shards :: shards()) -> {ok, binary()} | {error, any()}.
decode(_, _, _) ->
    not_loaded(?LINE).

-spec decode_gc(K :: pos_integer(), M :: pos_integer(), Shards :: shards()) -> {ok, binary()} | {error, any()}.
decode_gc(K, M, Shards) ->
    decode_gc(K, M, ceil(math:log2(K+M)), Shards).

-spec decode_gc(K :: pos_integer(), M :: pos_integer(), W :: pos_integer(), Shards :: shards()) -> {ok, binary()} | {error, any()}.
decode_gc(_, _, _, _) ->
    not_loaded(?LINE).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
