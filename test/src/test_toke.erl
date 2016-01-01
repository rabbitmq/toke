%%  The contents of this file are subject to the Mozilla Public License
%%  Version 1.1 (the "License"); you may not use this file except in
%%  compliance with the License. You may obtain a copy of the License
%%  at http://www.mozilla.org/MPL/
%%
%%  Software distributed under the License is distributed on an "AS IS"
%%  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%  the License for the specific language governing rights and
%%  limitations under the License.
%%
%%  The Original Code is Toke.
%%
%%  The Initial Developer of the Original Code is GoPivotal, Inc.
%%  Copyright (c) 2007-2016 Pivotal Software, Inc.  All rights reserved.
%%

-module(test_toke).

-compile([export_all]).

test() ->
    {ok, Toke} = toke_drv:start_link(),
    ok = toke_drv:new(Toke),

    {ok, Toke2} = toke_drv:start_link(),
    ok = toke_drv:new(Toke2),

    {ok, Toke3} = toke_drv:start_link(),
    ok = toke_drv:new(Toke3),

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
    ok = toke_drv:delete_if_value_eq(Toke, Ten, Ten),
    not_found = toke_drv:get(Toke, Ten),
    ok = toke_drv:delete(Toke, Ten),
    ok = toke_drv:insert(Toke, Ten, Ten),
    Ten = toke_drv:get(Toke, Ten),
    Nine = <<9:32/native>>,
    ok = toke_drv:insert(Toke, Ten, Nine),
    Nine = toke_drv:get(Toke, Ten),
    ok = toke_drv:delete_if_value_eq(Toke, Ten, Ten),
    Nine = toke_drv:get(Toke, Ten),
    ok = toke_drv:delete(Toke, Ten),
    not_found = toke_drv:get(Toke, Ten),
    ok = toke_drv:delete_if_value_eq(Toke, Ten, Ten),
    ok = toke_drv:delete_if_value_eq(Toke, Ten, Nine),
    ok = toke_drv:close(Toke),
    ok = toke_drv:delete(Toke),
    ok = toke_drv:stop(Toke),

    ok = toke_drv:set_cache(Toke2, 1000000),
    ok = toke_drv:tune(Toke2, 2000000, 5, 15, [large]),
    ok = toke_drv:open(Toke2, "/tmp/test", [read]),
    Nine = toke_drv:get(Toke2, Nine),
    ok = toke_drv:close(Toke2),
    ok = toke_drv:delete(Toke2),
    ok = toke_drv:stop(Toke2),

    ok = toke_drv:set_cache(Toke3, 1000000),
    ok = toke_drv:tune(Toke3, 2000000, 5, 15, [large]),
    ok = toke_drv:open(Toke3, "/tmp/test", [read]),
    Sum = (1000000*1000001) - 20,
    Sum = toke_drv:fold(fun (<<Key:32/native>>, <<Value:32/native>>, Acc)
                              when Key =:= Value ->
                                Key + Value + Acc end, 0, Toke3),
    ok = toke_drv:close(Toke3),
    ok = toke_drv:delete(Toke3),
    ok = toke_drv:stop(Toke3),

    passed.
