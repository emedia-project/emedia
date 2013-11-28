#!/bin/sh

# demonize : http://stackoverflow.com/questions/6830806/running-erlang-shell-as-a-deamon-service

get_db_path() {
  config_file_array='config_file_'
  config_file_1="/etc/emedia.conf" 
  config_file_2="/Library/Application Support/eMedia/emedia.conf" 
  config_file_3="~/Library/Application Support/eMedia/emedia.conf" 
  config_file_4="~/.emedia/emedia.conf" 
  config_file_5="emedia.conf"

  for i in $(seq 1 5); do
    the_content=$(echo $(eval echo \$$config_file_array${i}) | sed -e "s|^~|$HOME|")

    if [ -f "$the_content" ] ; then
      db_path=$(awk -F "=" '/^[ \t]*db_path/ {print $2}' "$the_content" | sed -e 's/#.*$//' | sed -e 's/^[ \t]*//' | sed -e 's/[ \t]*$//' | tail -1 | sed -e "s|^~|$HOME|")
      [ -d "$db_path" ] && DB_PATH=$(echo $db_path)
    fi
  done
}

get_db_path
if [ "$DB_PATH" = "" ] ; then
  DB_PATH=$(pwd)
fi

DB_FILE=$(echo "$DB_PATH/eMedia.mnesia")

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

MNESIA="-mnesia dir '$DB_FILE'"

#echo erl +pc unicode -pa ebin deps/*/ebin $NOSHELL $MNESIA -s emediaserver
erl +pc unicode -pa ebin deps/*/ebin $NOSHELL $MNESIA -s emediaserver
#erl -pa ebin deps/*/ebin $NOSHELL $MNESIA -s emediaserver
