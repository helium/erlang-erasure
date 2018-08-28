-module(erasure).

-export([encode/3,
         decode/3
        ]).

-on_load(init/0).

-define(APPNAME, erasure).
-define(LIBNAME, 'erasure').

-type shards() :: [{Index :: non_neg_integer(), TotalSize :: pos_integer(), binary()}, ...].

-spec encode(K :: pos_integer(), M :: pos_integer(), binary()) -> {ok, shards()}.
encode(_, _, _) ->
    not_loaded(?LINE).

-spec decode(K :: pos_integer(), M :: pos_integer(), Shards :: shards()) -> {ok, binary()} | {error, any()}.
decode(_, _, _) ->
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


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    Data = <<"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Morbi nec nisi interdum, ultricies mauris eget, congue ante. Fusce erat diam, lacinia eu volutpat ut, gravida quis justo. Maecenas sagittis, ligula.">>,
    {ok, Shards} = encode(5, 2, Data),
    ?assertEqual({ok, Data}, decode(5, 2, Shards)),
    ?assertEqual({ok, Data}, decode(5, 2, lists:sublist(Shards, 5))),
    ?assertEqual({ok, Data}, decode(5, 2, lists:reverse(lists:sublist(Shards, 5)))),
    ?assertMatch({error, _}, decode(5, 2, lists:sublist(Shards, 4))),
    ?assertMatch({error, _}, decode(5, 2, lists:sublist(Shards, 4) ++ [hd(Shards)])),
    ok.

-endif.
