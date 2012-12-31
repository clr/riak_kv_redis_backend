%%% @copyright 2012 Casey Rosenthal, All rights reserved. Open source, BSD License
%%% @version 0.1
%%%
%%%
%%% Copyright (c) 2012 Casey Rosenthal
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%% 3. Neither the name of the copyright holder nor the names of contributors
%%%    may be used to endorse or promote products derived from this software
%%%    without specific prior written permission.
%%
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

-define(API_VERSION, 1).
-define(CAPABILITIES, []).

-record(state, {}).

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
    {ok, #state{}}.

%% @doc Stop the redis backend
stop(_) -> ok.

%% @doc Retrieve an object from the redis backend
get(Bucket, Key, State) ->
    Uid = term_to_binary({Bucket, Key}),
		case erl_hiredis:get(Uid) of
        {error, _} -> {error, not_found, State};
        {ok, Value} -> {ok, Value, State}
    end.

%% @doc Insert an object into the redis backend.
put(Bucket, Key, _IndexSpecs, Value, State) ->
    Uid = term_to_binary({Bucket, Key}),
    erl_hiredis:put(list_to_binary("__bucket__"++binary_to_list(Bucket)), Key, list_to_binary("__world"), Uid, Value),
    {ok, State}.

%% @doc Delete an object from the redis backend
delete(Bucket, Key, _IndexSpecs, State) ->
    Uid = term_to_binary({Bucket, Key}),
    erl_hiredis:delete(list_to_binary("__bucket__"++binary_to_list(Bucket)), Key, list_to_binary("__world"), Uid),
    {ok, State}.

%% @doc Delete all objects from this redis backend
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

