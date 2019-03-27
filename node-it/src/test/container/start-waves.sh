#!/bin/bash

trap 'kill -TERM $PID' TERM INT
echo Options: $WAVES_OPTS
java $WAVES_OPTS -cp "/opt/waves/lib/*" com.wavesplatform.Application /opt/waves/template.conf &
PID=$!
wait $PID
trap - TERM INT
wait $PID
EXIT_STATUS=$?
