#!/bin/bash

### BLL version lyxass and utilities

export LYNX_BASEDIR=`pwd`
export LYNX_BIN="$LYNX_BASEDIR/bin"
export LYNX_LYXASS="$LYNX_BASEDIR/lyxass"

export PATH=$PATH:$LYNX_BIN

echo "LYNX_BASEDIR=$LYNX_BASEDIR"
echo "LYNX_BIN=$LYNX_BIN"


### BLL version newcc65
export LYNX_NEWCC="$LYNX_BASEDIR/newcc65"
### export PATH=$PATH:${LYNX_NEWCC}/bin

echo "LYNX_NEWCC=$LYNX_NEWCC"

## now this is for the cc65.org version...
export LYNX_CC65_REMAKE="$LYNX_BASEDIR/cc65_remake"
export CC65_HOME=${LYNX_CC65_REMAKE}/lib/cc65
export CC65_BIN=${LYNX_CC65_REMAKE}/bin
echo "LYNX_CC65_REMAKE=$LYNX_CC65_REMAKE"
echo "CC65_HOME=${CC65_HOME}"

export CA65_INC=${CC65_HOME}/asminc/
export CC65_INC=${CC65_HOME}/include/
export LD65_CFG=${CC65_HOME}/cfg/
export CC65_LIB=${CC65_HOME}/lib/
export LD65_LIB=${CC65_HOME}/lib/
export LD65_OBJ=${CC65_HOME}/obj/

export PATH=$PATH:${CC65_BIN}

PS1="\[\e[33;1m\]\u\[\e[32;1m\]@\[\e[36;1m\]\H \[\e[31;1m\]\w\[\e[32;1m\] (LYNX)\n$ \[\e[0m\]"

cd $LYNX_BASEDIR
