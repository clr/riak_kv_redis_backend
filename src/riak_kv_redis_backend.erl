%% -------------------------------------------------------------------
%%
%% riak_redis_backend: storage engine using ETS tables
%%
%% Copyright (c) 2007-2011 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc riak_kv_redis_backend is a Riak storage backend that uses ets
%% tables to store all data in redis.
%%
%% === Configuration Options ===
%%
%% The following configuration options are available for the redis backend.
%% The options should be specified in the `redis_backend' section of your
%% app.config file.
%%
%% <ul>
%% <li>`ttl' - The time in seconds that an object should live before being expired.</li>
%% <li>`max_redis' - The amount of memory in megabytes to limit the backend to.</li>
%% <li>`test' - When true, allow public access to ETS tables so they can be cleared efficiently.</li>
%% </ul>
%%

-module(riak_kv_redis_backend).
-behavior(riak_kv_backend).

%% KV Backend API
-export([api_version/0,
         capabilities/1,
         capabilities/2,
         start/2,
         stop/1,
         get/3,
         put/5,
         delete/4,
         drop/1,
         fold_buckets/4,
         fold_keys/4,
         fold_objects/4,
         is_empty/1,
         status/1,
         callback/3]).

%% "Testing" backend API
-define(API_VERSION, 1).
-define(CAPABILITIES, []).

-record(state, {
                partition :: integer()}).

-type state() :: #state{}.
-type config() :: [].

%% ===================================================================
%% Public API
%% ===================================================================

%% KV Backend API

%% @doc Return the major version of the
%% current API.
-spec api_version() -> {ok, integer()}.
api_version() ->
    {ok, ?API_VERSION}.

%% @doc Return the capabilities of the backend.
-spec capabilities(state()) -> {ok, [atom()]}.
capabilities(_) ->
    {ok, ?CAPABILITIES}.

%% @doc Return the capabilities of the backend.
-spec capabilities(riak_object:bucket(), state()) -> {ok, [atom()]}.
capabilities(_, _) ->
    {ok, ?CAPABILITIES}.

%% @doc Start the redis backend
-spec start(integer(), config()) -> {ok, state()}.
start(Partition, _Config) ->
    {ok, #state{
        partition=3}}.

%% @doc Stop the redis backend
-spec stop(state()) -> ok.
stop(_) -> ok.

%% @doc Retrieve an object from the redis backend
-spec get(riak_object:bucket(), riak_object:key(), state()) ->
                 {ok, any(), state()} |
                 {ok, not_found, state()} |
                 {error, term(), state()}.
get(Bucket, Key, State) ->
    Uid = term_to_binary({Bucket, Key}),
		case erl_hiredis:get(Uid) of
        {error, _} -> {error, not_found, State};
        {ok, Value} -> {ok, Value, State}
    end.

%% @doc Insert an object into the redis backend.
put(Bucket, Key, _IndexSpecs, Value, State) ->
    %erl_hiredis:put(<<"b">>, <<"k">>, <<"world">>, <<"UNIQUE">>, <<"VALUE">>),
    Uid = term_to_binary({Bucket, Key}),
    erl_hiredis:put(list_to_binary("__bucket__"++binary_to_list(Bucket)), Key, list_to_binary("__world"), Uid, Value),
    {ok, State}.

%% @doc Delete an object from the redis backend
delete(Bucket, Key, _IndexSpecs, State) ->
    Uid = term_to_binary({Bucket, Key}),
    erl_hiredis:delete(list_to_binary(binary_to_list(Bucket)++":"), Key, list_to_binary("world:"), Uid),
    {ok, State}.

%% @doc Delete all objects from this redis backend
-spec drop(state()) -> {ok, state()}.
drop(State) -> {ok, State}.

%% @doc Returns true if this redis backend contains any
%% non-tombstone values; otherwise returns false.
-spec is_empty(state()) -> boolean().
is_empty(_State) -> true.

%% @doc Get the status information for this redis backend
-spec status(state()) -> [{atom(), term()}].
status(_State) -> [{data_table_status, true},
                   {index_table_status, true}].

%% @doc Register an asynchronous callback
-spec callback(reference(), any(), state()) -> {ok, state()}.
callback(_Ref, _Msg, State) ->
    {ok, State}.

%% @doc Fold over all the buckets.
-spec fold_buckets(riak_kv_backend:fold_buckets_fun(),
                   any(),
                   [],
                   state()) -> {ok, any()}.
fold_buckets(_FoldBucketsFun, _Acc, _Opts, _State) ->
    {ok, true}.

%% @doc Fold over all the keys for one or all buckets.
-spec fold_keys(riak_kv_backend:fold_keys_fun(),
                any(),
                [{atom(), term()}],
                state()) -> {ok, term()} | {async, fun()}.
fold_keys(_FoldKeysFun, _Acc, _Opts, _State) ->
    {ok, true}.

%% @doc Fold over all the objects for one or all buckets.
-spec fold_objects(riak_kv_backend:fold_objects_fun(),
                   any(),
                   [{atom(), term()}],
                   state()) -> {ok, any()} | {async, fun()}.
fold_objects(_FoldObjectsFun, _Acc, _Opts, _State) ->
    {ok, true}.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @TODO Some of these implementations may be suboptimal.
%% Need to do some measuring and testing to refine the
%% implementations.

%% ===================================================================
%% EUnit tests
%% ===================================================================

-ifdef(TEST).

simple_test_() ->
    riak_kv_backend:standard_test(?MODULE, []).

ttl_test_() ->
    Config = [{ttl, 15}],
    {ok, State} = start(42, Config),

    Bucket = <<"Bucket">>,
    Key = <<"Key">>,
    Value = <<"Value">>,

    [
     %% Put an object
     ?_assertEqual({ok, State}, put(Bucket, Key, [], Value, State)),
     %% Wait 1 second to access it
     ?_assertEqual(ok, timer:sleep(1000)),
     ?_assertEqual({ok, Value, State}, get(Bucket, Key, State)),
     %% Wait 3 seconds and access it again
     ?_assertEqual(ok, timer:sleep(3000)),
     ?_assertEqual({ok, Value, State}, get(Bucket, Key, State)),
     %% Wait 15 seconds and it should expire
     {timeout, 30000, ?_assertEqual(ok, timer:sleep(15000))},
     %% This time it should be gone
     ?_assertEqual({error, not_found, State}, get(Bucket, Key, State))
    ].

%% @private
max_redis_test_() ->
    %% Set max size to 1.5kb
    Config = [{max_redis, 1.5 * (1 / 1024)}],
    {ok, State} = start(42, Config),

    Bucket = <<"Bucket">>,
    Key1 = <<"Key1">>,
    Value1 = list_to_binary(string:copies("1", 1024)),
    Key2 = <<"Key2">>,
    Value2 = list_to_binary(string:copies("2", 1024)),

    %% Write Key1 to the datastore
    {ok, State1} = put(Bucket, Key1, [], Value1, State),
    timer:sleep(timer:seconds(1)),
    %% Write Key2 to the datastore
    {ok, State2} = put(Bucket, Key2, [], Value2, State1),

    [
     %% Key1 should be kicked out
     ?_assertEqual({error, not_found, State2}, get(Bucket, Key1, State2)),
     %% Key2 should still be present
     ?_assertEqual({ok, Value2, State2}, get(Bucket, Key2, State2))
    ].

-ifdef(EQC).

eqc_test_() ->
    {spawn,
     [{inorder,
       [{setup,
         fun setup/0,
         fun cleanup/1,
         [
          {timeout, 60000,
           [?_assertEqual(true,
                          backend_eqc:test(?MODULE, true))]}
         ]}]}]}.

setup() ->
    application:load(sasl),
    application:set_env(sasl, sasl_error_logger, {file, "riak_kv_redis_backend_eqc_sasl.log"}),
    error_logger:tty(false),
    error_logger:logfile({open, "riak_kv_redis_backend_eqc.log"}),
    ok.

cleanup(_) ->
    ok.

-endif. % EQC

-endif. % TEST
