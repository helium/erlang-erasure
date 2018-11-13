#!/bin/sh

GF_VERSION="a6862d10c9db467148f20eef2c6445ac9afd94d8"
JE_VERSION="de1739cc8483696506829b52e7fda4f6bb195e6a"


if [ ! -d c_src/gf-complete ]; then
    git clone http://github.com/ceph/gf-complete.git c_src/gf-complete
fi

cd c_src/gf-complete

CURRENT_VERSION=`git rev-parse HEAD`

if [ ! "$GF_VERSION" = "$CURRENT_VERSION" ]; then
    git clean -ddxxff
    git fetch
    git checkout $GF_VERSION
fi

if [ ! -d build ]; then
    mkdir build
    ./autogen.sh
    ./configure --prefix=`pwd`/build --with-pic $CONFIGURE_ARGS
fi

if [ ! -f build/lib/libgf_complete.a ]; then
    make -j
    make install
fi

cd ../..

if [ ! -d c_src/jerasure ]; then
    git clone http://github.com/ceph/jerasure.git c_src/jerasure
fi

cd c_src/jerasure

CURRENT_VERSION=`git rev-parse HEAD`

if [ ! "$JE_VERSION" = "$CURRENT_VERSION" ]; then
    git clean -ddxxff
    git fetch
    git checkout $JE_VERSION
fi

if [ ! -d build ]; then
    mkdir build
    autoreconf --force --install
    LDFLAGS="-L`pwd`/../gf-complete/build/lib -fPIC" CPPFLAGS="-I`pwd`/../gf-complete/build/include" ./configure --prefix=`pwd`/build --enable-static --with-pic $CONFIGURE_ARGS
fi

if [ ! -f build/lib/libJerasure.a ]; then
    make -j
    make install
fi
