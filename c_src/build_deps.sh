#!/bin/sh

set -e

VSN=0.11.0

[ `basename $PWD` = c_src ] || cd c_src

case "$1" in
    clean)
        rm -rf hiredis
        ;;

    *)
        if [ ! -d hiredis ] || [ ! -f hiredis/libhiredis.a ]; then
            tar -xzf v${VSN}.tar.gz
            mv hiredis-$VSN hiredis
            ( cd hiredis && make CFLAGS+="$DRV_CFLAGS" )
        fi
        ;;
esac
