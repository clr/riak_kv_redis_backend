# riak_kv_redis_backend

## Description

This wraps the C library Hiredis as an Erlang NIF, and uses that to
connect to Redis over Unix Domain Socket to use as a storage backend for
Riak.

THIS CODE IS EXPERIMENTAL AND UGLY.

As evidenced by the lack of tests, this was written as a spike.  My C is
RAF, and this is a raw learning experience for me.

## Performance

I get at least 80% of the ops/sec as with using the memory backend in
Riak.  This could be improved by modifying the way the pool connects to
the driver, or by switching to the async C code in Hiredis.

## Why

* Distributed Redis
 
* Distributed PUB/SUB

* A memory backend with asynchronous writes to disk

* Everything that lua scripting offers

## Building and Installing

Have a Riak instance [installed](http://docs.basho.com/riak/latest/tutorials/installation/) somewhere.

Have a Redis server running on unix socket "/tmp/redis.sock" by
adjusting your [redis.conf](https://raw.github.com/antirez/redis/2.6/redis.conf)

Clone into this repo and

    rebar compile

If everything compiles successfully, then copy the entire directory into
the ./lib directory of your Riak instance.

Now edit ./etc/app.config and switch the value of storage_backend to riak_kv_redis_backend and you are done. Start Riak and off you go.

## Verify

Verify it is working by curling a value at Riak:

    curl -X POST http://127.0.0.1:8098/buckets/pizza/keys/pizza -d 'PIZZA!' -H 'content-type: text/plain'    

Connect to redis-cli and list keys:

    KEYS *

You should see three keys listed:

    1) "\x83h\x02m\x00\x00\x00\x05pizzam\x00\x00\x00\x05pizza"
    2) "__world"
    3) "__bucket__pizza"

## Broken

Folding over keys currently does not work.  This is because I just
didn't get to it yet.  However, I did write the bucket names and keys
into separate sets in Redis.  This is so that we have the full
performance impact of writing that extra data.

## Acknowledgement

I am trying to learn to write a NIF mostly by examining code from the great [Steve
Vinoski](https://github.com/vinoski); however, all errors in both code and style are obviously mine
and mine alone.

Also special thanks to Jesse Williamson
and [Sean Cribbs](https://github.com/seancribbs)
for helping me figure out my first NIF. :-)

## Help

Want to help? Fork it and bork it.
