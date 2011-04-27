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
%%  The Initial Developer of the Original Code is VMware, Inc.
%%  Copyright (c) 2009-2011 VMware, Inc.  All rights reserved.
%%

-module(toke_drv).
-behaviour(gen_server).

-export([start_link/0]).

-export([new/1, delete/1, tune/5, set_cache/2, set_xm_size/2, set_df_unit/2,
         open/3, close/1, insert/3, insert_new/3, insert_concat/3,
         insert_async/3, delete/2, delete_if_value_eq/3, get/2, fold/3,
         update_atomically/3, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

%% Tokyo Cabinet driver for Erlang

%% Only implements a skeleton of the hash table API.  All keys and
%% values must be binaries. Whilst the driver is robust, there is no
%% attempt to check state. Thus if you, eg, call tune after opening
%% the database, you will get an invalid state error from Tokyo
%% Cabinet rather than something more meaningful.

-define(LIBNAME, "libtoke").

-define(TOKE_NEW,           0). %% KEEP IN SYNC WITH TOKE.H
-define(TOKE_DEL,           1).
-define(TOKE_TUNE,          2).
-define(TOKE_SET_CACHE,     3).
-define(TOKE_SET_XM_SIZE,   4).
-define(TOKE_SET_DF_UNIT,   5).
-define(TOKE_OPEN,          6).
-define(TOKE_CLOSE,         7).
-define(TOKE_INSERT,        8).
-define(TOKE_INSERT_NEW,    9).
-define(TOKE_INSERT_CONCAT, 10).
-define(TOKE_INSERT_ASYNC,  11).
-define(TOKE_DELETE,        12).
-define(TOKE_DELETE_IF_EQ,  13).
-define(TOKE_GET,           14).
-define(TOKE_GET_ALL,       15).

%% KEEP IN SYNC WITH TOKE.H
-define(TUNE_KEYS,          [large, deflate, bzip, tcbs, excodec]).
-define(OPEN_KEYS,          [read, write, create, truncate, no_lock,
                             lock_no_block, sync_on_transaction]).

%%----------------------------------------------------------------------------
%% Public API
%%----------------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% Set up the driver with a new TCHDB object.
new(Pid) ->
    gen_server:call(Pid, new, infinity).

%% Destroy the driver's TCHDB object.
delete(Pid) ->
    gen_server:call(Pid, delete, infinity).

%% Tune the driver's TCHDB Object. Opts :: [ large, deflate, bzip,
%%                                           tcbs, excodec ]
%% Don't do this after opening the db.
tune(Pid, BNum, APow, FPow, Opts) ->
    gen_server:call(Pid, {tune, BNum, APow, FPow, Opts}, infinity).

%% Set the number of records to cache. Don't do this after opening the db.
set_cache(Pid, RecordsToCache) ->
    gen_server:call(Pid, {set_cache, RecordsToCache}, infinity).

%% Set the extra amount of memory mapped in. Don't do this after opening the db.
set_xm_size(Pid, ExtraMappedMemory) ->
    gen_server:call(Pid, {set_xm_size, ExtraMappedMemory}, infinity).

%% Set the steps between auto defrag. Don't do this after opening the db.
set_df_unit(Pid, DefragStepUnit) ->
    gen_server:call(Pid, {set_df_unit, DefragStepUnit}, infinity).

%% Open a db. Modes :: [ read, write, create, truncate, no_lock,
%%                       lock_no_block, sync_on_transaction ]
open(Pid, Path, Modes) ->
    gen_server:call(Pid, {open, Path, Modes}, infinity).

%% Close an open db.
close(Pid) ->
    gen_server:call(Pid, close, infinity).

%% Insert. If the key already exists, value is updated.
insert(Pid, Key, Value) when is_binary(Key) andalso is_binary(Value) ->
    gen_server:call(Pid, {insert, Key, Value}, infinity).

%% Insert new. If the key already exists, the old value is silently kept.
insert_new(Pid, Key, Value) when is_binary(Key) andalso is_binary(Value) ->
    gen_server:call(Pid, {insert_new, Key, Value}, infinity).

%% Concatenate the supplied value with an existing value for this key.
insert_concat(Pid, Key, Value) when is_binary(Key) andalso is_binary(Value) ->
    gen_server:call(Pid, {insert_concat, Key, Value}, infinity).

%% Asynchronously insert. If the key already exists, value is updated.
insert_async(Pid, Key, Value) when is_binary(Key) andalso is_binary(Value) ->
    gen_server:cast(Pid, {insert_async, Key, Value}).

%% Delete a key from the db.
delete(Pid, Key) when is_binary(Key) ->
    gen_server:call(Pid, {delete, Key}, infinity).

%% Delete the key iff the current value matches the supplied value.
delete_if_value_eq(Pid, Key, Obj) when is_binary(Key) andalso is_binary(Obj) ->
    gen_server:call(Pid, {delete_if_value_eq, Key, Obj}, infinity).

%% Fetch a key from the db. Returns 'not_found' on occasion.
get(Pid, Key) when is_binary(Key) ->
    gen_server:call(Pid, {get, Key}, infinity).

%% Fold over every value in the db.
fold(Fun, Init, Pid) ->
    gen_server:call(Pid, {fold, Fun, Init}, infinity).

%% Atomically modify the specified value.
update_atomically(Pid, Key, Fun) ->
    gen_server:call(Pid, {update_atomically, Key, Fun}, infinity).

%% Stop the driver and close the port.
stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

%%----------------------------------------------------------------------------
%% Gen_server callbacks
%%----------------------------------------------------------------------------

init([]) ->
    erl_ddll:start(),
    {file, Path} = code:is_loaded(?MODULE),
    Dir = filename:join(filename:dirname(Path), "../priv"),
    case erl_ddll:load_driver(Dir, ?LIBNAME) of
        ok                 -> ok;
        {error, permanent} -> ok %% it's already loaded
    end,
    ok = erl_ddll:load_driver(Dir, ?LIBNAME),
    Port = open_port({spawn_driver, ?LIBNAME}, [binary, stream]),
    {ok, Port}.

handle_call(new, _From, Port) ->
    port_command(Port, <<?TOKE_NEW/native>>),
    simple_reply(Port);

handle_call(delete, _From, Port) ->
    port_command(Port, <<?TOKE_DEL/native>>),
    simple_reply(Port);

%% int64_t bnum, int8_t apow, int8_t fpow, uint8_t opts
handle_call({tune, BNum, APow, FPow, Opts}, _From, Port) ->
    Opt = build_bit_mask(Opts, ?TUNE_KEYS),
    port_command(Port, <<?TOKE_TUNE/native,
                        BNum:64/signed-integer-native,
                        APow:8/signed-integer-native,
                        FPow:8/signed-integer-native,
                        Opt:8/native>>),
    simple_reply(Port);

%% int32_t rcnum
handle_call({set_cache, RecordCacheNum}, _From, Port) ->
    port_command(Port, <<?TOKE_SET_CACHE/native,
                        RecordCacheNum:32/signed-integer-native>>),
    simple_reply(Port);

%% int64_t xmsiz
handle_call({set_xm_size, ExtraMappedMemory}, _From, Port) ->
    port_command(Port, <<?TOKE_SET_XM_SIZE/native,
                        ExtraMappedMemory:64/signed-integer-native>>),
    simple_reply(Port);

%% int32_t dfunit
handle_call({set_df_unit, DefragStepUnit}, _From, Port) ->
    port_command(Port, <<?TOKE_SET_DF_UNIT/native,
                        DefragStepUnit:32/signed-integer-native>>),
    simple_reply(Port);

handle_call({open, Path, Modes}, _From, Port) ->
    Mode = build_bit_mask(Modes, ?OPEN_KEYS),
    port_command(Port, <<?TOKE_OPEN/native, (length(Path)):64/native,
                        (list_to_binary(Path))/binary, Mode:8/native>>),
    simple_reply(Port);

handle_call(close, _From, Port) ->
    port_command(Port, <<?TOKE_CLOSE/native>>),
    simple_reply(Port);

handle_call({insert, Key, Value}, _From, Port) ->
    insert_sync(Port, ?TOKE_INSERT, Key, Value);

handle_call({insert_new, Key, Value}, _From, Port) ->
    insert_sync(Port, ?TOKE_INSERT_NEW, Key, Value);

handle_call({insert_concat, Key, Value}, _From, Port) ->
    insert_sync(Port, ?TOKE_INSERT_CONCAT, Key, Value);

handle_call({delete, Key}, _From, Port) ->
    KeySize = size(Key),
    port_command(Port, <<?TOKE_DELETE/native, KeySize:64/native, Key/binary>>),
    simple_reply(Port);

handle_call({delete_if_value_eq, Key, Obj}, _From, Port) ->
    insert_sync(Port, ?TOKE_DELETE_IF_EQ, Key, Obj);

handle_call({get, Key}, _From, Port) ->
    {reply, internal_get(Key, Port), Port};

handle_call({fold, Fun, Init}, _From, Port) ->
    port_command(Port, <<?TOKE_GET_ALL/native>>),
    Result = receive_all(Fun, Init),
    {reply, Result, Port};

handle_call({update_atomically, Key, Fun}, _From, Port) ->
    case internal_get(Key, Port) of
        not_found -> ok;
        Entry -> insert_async(Port, ?TOKE_INSERT_ASYNC, Key, Fun(Entry))
    end,
    {reply, ok, Port};

handle_call(stop, _From, Port) ->
    {stop, normal, ok, Port}. %% gen_server now calls terminate/2

handle_cast({insert_async, Key, Value}, Port) ->
    insert_async(Port, ?TOKE_INSERT_ASYNC, Key, Value).

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, Port) ->
    port_close(Port).

%%----------------------------------------------------------------------------
%% Internal helpers
%%----------------------------------------------------------------------------

internal_get(Key, Port) ->
    KeySize = size(Key),
    port_command(Port, <<?TOKE_GET/native, KeySize:64/native, Key/binary>>),
    receive {toke_reply, Result} -> Result end.

build_bit_mask(Flags, Keys) ->
    {Int, _Index} =
        lists:foldl(fun (Key, {Acc, Index}) ->
                            {case proplists:get_bool(Key, Flags) of
                                 true  -> Acc bor (1 bsl Index);
                                 false -> Acc
                             end, 1 + Index}
                    end, {0, 0}, Keys),
    Int.

insert_async(Port, Command, Key, Value) ->
    KeySize = size(Key),
    ValueSize = size(Value),
    port_command(Port, <<Command/native, KeySize:64/native,
                        Key/binary, ValueSize:64/native, Value/binary>>),
    {noreply, Port}.

insert_sync(Port, Command, Key, Value) ->
    insert_async(Port, Command, Key, Value),
    simple_reply(Port).

simple_reply(Port) ->
    {reply, receive {toke_reply, Result} -> Result end, Port}.

receive_all(Fun, Acc) ->
    receive
        {toke_reply, ok}         -> Acc;
        {toke_reply, Key, Value} -> receive_all(Fun, Fun(Key, Value, Acc))
    end.
