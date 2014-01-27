#!/bin/sh

EBIN="ebin apps/*/ebin deps/*/ebin"
ETC="etc"

help() {
  MESSAGE=$1
  if [ "x$MESSAGE" != "x" ] ; then
    echo $MESSAGE
  fi
  echo "Usage : start.sh [options]"
  echo ""
  echo "Options :"
  echo "  -d --daemon [start|stop|status]    : Daemonize"
  echo "  -c --console                       : Run in console mode"
  echo "  -C --compile                       : Compile code before run"
  echo "  -K --clean                         : Clean and compile code before run"
  echo "     --no-ssdp                       : Do not start the SSDP server"
  echo "  -h --help                          : Display this message"
}

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

NOSHELL="-noshell"
COMPILE=false
CLEAN=false
DAEMON=none
NOSSDP="true"

while (( "$#" )); do
  case $1 in
    -c|--console)
      NOSHELL="" ;;
    -C|--compile)
      COMPILE=true ;;
    -k|--clean)
      CLEAN=true ;;
    -h|--help)
      help ; exit 0 ;;
    -d|--daemon)
      shift ; NOSHELL="-noshell" ; DAEMON=$1 ;;
    --no-ssdp)
      NOSSDP="false" ;;
    *)
      help ; exit 0 ;;
  esac
  shift
done

if [ $CLEAN = true ] ; then
  ./rebar clean
fi
if [ $COMPILE = true ] ; then
  ./rebar get-deps
  ./rebar compile
fi

MNESIA="-mnesia dir '$DB_FILE'"

case $DAEMON in
  none)
    erl +pc unicode -pa $EBIN -config $ETC/emedia-app.config $NOSHELL $MNESIA -s emediaserver start $NOSSDP;;
  start)
    # http://mindeavor.com/blog/running-erlang-in-the-background-tutorial
    echo "start with run_erl -daemon /var/run/... \"...\"" ;;
  stop)
    echo "stop" ;;
  status)
    echo "status" ;;
esac
