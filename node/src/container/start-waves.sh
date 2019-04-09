#!/bin/bash

trap 'kill -TERM $PID' TERM INT
echo Config file: $WAVES_CONFIG_FILE
echo Options: $WAVES_OPTS
java $WAVES_OPTS -cp "/opt/waves/lib/*" com.wavesplatform.Application $WAVES_CONFIG_FILE &
PID=$!
wait $PID
trap - TERM INT
wait $PID
EXIT_STATUS=$?
