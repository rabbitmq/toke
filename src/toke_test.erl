-module(toke_test).

-compile([export_all]).

test() ->
    {ok, Toke} = toke_drv:start_link(),
    ok = toke_drv:new(Toke),
    ok = toke_drv:set_cache(Toke, 1000000),
    ok = toke_drv:tune(Toke, 2000000, 5, 15, [large]),
    ok = toke_drv:open(Toke, "/tmp/test", [read, write, create, truncate]),
    ok = lists:foldl(
           fun (Num, ok) ->
                   B = <<Num:32/native>>,
                   toke_drv:insert_async(Toke, B, B)
           end, ok, lists:seq(1,1000000)),
    Ten = <<10:32/native>>,
    Ten = toke_drv:get(Toke, Ten),
    Nine = <<9:32/native>>,
    ok = toke_drv:insert(Toke, Ten, Nine),
    Nine = toke_drv:get(Toke, Ten),
    ok = toke_drv:delete(Toke, Ten),
    not_found = toke_drv:get(Toke, Ten),
    ok = toke_drv:close(Toke),
    ok = toke_drv:delete(Toke),
    ok = toke_drv:stop(Toke),

    {ok, Toke2} = toke_drv:start_link(),
    ok = toke_drv:new(Toke2),
    ok = toke_drv:set_cache(Toke2, 1000000),
    ok = toke_drv:tune(Toke2, 2000000, 5, 15, [large]),
    ok = toke_drv:open(Toke2, "/tmp/test", [read]),
    Nine = toke_drv:get(Toke2, Nine),
    ok = toke_drv:close(Toke2),
    ok = toke_drv:delete(Toke2),
    ok = toke_drv:stop(Toke2),

    {ok, Toke3} = toke_drv:start_link(),
    ok = toke_drv:new(Toke3),
    ok = toke_drv:set_cache(Toke3, 1000000),
    ok = toke_drv:tune(Toke3, 2000000, 5, 15, [large]),
    ok = toke_drv:open(Toke3, "/tmp/test", [read]),
    Sum = trunc((1000000*1000001)/2) - 10,
    Sum = toke_drv:fold(fun (<<Value:32/native>>, Acc) -> Value + Acc end, 0, Toke3),
    ok = toke_drv:close(Toke3),
    ok = toke_drv:delete(Toke3),
    ok = toke_drv:stop(Toke3),

    passed.
