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

#include "config.h"
#if HAVE_STDINT_H
#include <stdint.h>
#elif HAVE_INTTYPES_H
#include <inttypes.h>
#endif
#include <string.h>
#include "erl_nif.h"

#include "hiredis/hiredis.h"


static ERL_NIF_TERM
ping(ErlNifEnv* env, int argc)
{
    redisContext *c;
    redisReply *reply;
    c = redisConnectUnixNonBlock((char*)"/tmp/redis.sock");
    if (c->err) {
        printf("Connection error: %s\n", c->errstr);
        exit(1);
    }

    reply = redisCommand(c,"PING");
    printf("PING: %s\n", reply->str);
    freeReplyObject(reply);
		
	  return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_ulong(env, 1));
}

static ErlNifFunc funcs[] = {
    {"ping", 0, ping},
};

static int
nifload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    *priv_data = enif_open_resource_type(
        env,
        NULL,
        "erl_hiredis_context",
        NULL,
        ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
        NULL
    );
    return 0;
}

ERL_NIF_INIT(erl_hiredis, funcs, nifload, NULL, NULL, NULL)
