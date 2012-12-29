/*
 * Copyright (c) 2012 Casey Rosenthal
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holder nor the names of contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTOR(S) ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTOR(S) BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <string.h>
#include "erl_nif.h"

#include "hiredis/hiredis.h"

#define MAXBUFLEN       1024

static ERL_NIF_TERM
ping(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    redisContext *c;
    redisReply *reply;

    c = redisConnectUnix((const char*)"/tmp/redis.sock");

    if (c->err) {
				return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "redis_connection_error"));
    }

    reply = redisCommand(c,"PING");
    freeReplyObject(reply);
    redisFree(c);

		return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "pong"));
}

static ERL_NIF_TERM
get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    redisContext *c;
    redisReply *reply;

		ErlNifBinary uid, ibin;
    if (!enif_inspect_binary(env, argv[0], &uid))    return enif_make_badarg(env);

    c = redisConnectUnix((const char*)"/tmp/redis.sock");
    if (c->err) {
				return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "redis_connection_error"));
    }

    reply = redisCommand(c, "GET %b", uid.data, uid.size);
    redisFree(c);

		if(reply->type == REDIS_REPLY_NIL){
      freeReplyObject(reply);
			return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "nil_reply"));
		} else {
			enif_alloc_binary(reply->len, &ibin);
			memcpy(ibin.data, reply->str, reply->len);
      freeReplyObject(reply);
			return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &ibin));
		}
}

static ERL_NIF_TERM
put(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    redisContext *c;
    redisReply *reply;

		ErlNifBinary bucket, key, uid, world, val;
    if (!enif_inspect_binary(env, argv[0], &bucket)) return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[1], &key))    return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[2], &world))  return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[3], &uid))    return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[4], &val))    return enif_make_badarg(env);

    c = redisConnectUnix((const char*)"/tmp/redis.sock");
    if (c->err) {
				return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "redis_connection_error"));
    }

    redisAppendCommand(c, "SET %b %b", uid.data, uid.size, val.data, val.size);
    redisAppendCommand(c, "SADD %b %b", bucket.data, bucket.size, key.data, key.size);
    redisAppendCommand(c, "SADD %b %b", world.data, world.size, bucket.data, bucket.size);
    redisGetReply(c, (void **) &reply);
    freeReplyObject(reply);
    redisFree(c);

		return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "put"));
}

static ERL_NIF_TERM
delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    redisContext *c;
    redisReply *reply;

		ErlNifBinary bucket, key, uid, world;
    if (!enif_inspect_binary(env, argv[0], &bucket)) return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[1], &key))    return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[2], &world))  return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[3], &uid))    return enif_make_badarg(env);

    c = redisConnectUnix((const char*)"/tmp/redis.sock");
    if (c->err) {
				return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "redis_connection_error"));
    }

    redisAppendCommand(c, "DEL %b", uid.data, uid.size);
    redisAppendCommand(c, "SREM %b %b", bucket.data, bucket.size, key.data, key.size);
    redisAppendCommand(c, "SREM %b %b", world.data, world.size, bucket.data, bucket.size);
    redisGetReply(c, (void **) &reply);
    freeReplyObject(reply);
    redisFree(c);

		return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "put"));
}

static ERL_NIF_TERM
drop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    redisContext *c;
    redisReply *reply;

    c = redisConnectUnix((const char*)"/tmp/redis.sock");

    if (c->err) {
				return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "redis_connection_error"));
    }

    reply = redisCommand(c,"FLUSHALL");
    freeReplyObject(reply);
    redisFree(c);

		return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "drop"));
}

static ErlNifFunc funcs[] = {
    {"ping",   0, ping},
    {"get",    1, get},
    {"put",    5, put},
    {"delete", 4, delete},
    {"drop",   0, drop},
};

static int
nifload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

ERL_NIF_INIT(erl_hiredis, funcs, nifload, NULL, NULL, NULL)
