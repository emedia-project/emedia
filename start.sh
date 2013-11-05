#!/bin/sh

OPTS=$*

NOSHELL="-noshell"
COMPILE=false
CLEAN=false

for OPT in ${OPTS} ; do
  if [ "$OPT" = "-console" ] ; then
    NOSHELL=""
  fi
  if [ "$OPT" = "-compile" ] ; then
    COMPILE=true
  fi
  if [ "$OPT" = "-clean" ] ; then
    CLEAN=true
  fi
done

if [ $CLEAN = true ] ; then
  ./rebar clean
fi
if [ $COMPILE = true ] ; then
  ./rebar compile
fi

#MNESIA=-mnesia dir '"DEBServer.mnesia"'

erl -pa ebin deps/*/ebin $NOSHELL $MNESIA -s emediaserver
