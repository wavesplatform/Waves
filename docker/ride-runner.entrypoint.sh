#!/bin/bash
shopt -s nullglob

JAVA_OPTS="-javaagent:${RIDE_INSTALL_PATH}/kanela-agent/kanela-agent-1.0.16.jar
  -server
  -XX:+ExitOnOutOfMemoryError
  -XX:+HeapDumpOnOutOfMemoryError
  -XX:HeapDumpPath=${RDATA}/heap-dumps/on-exit
  -XX:+ParallelRefProcEnabled
  -XX:+UseStringDeduplication
  -Xmx${RIDE_HEAP_SIZE}
  -XX:MaxMetaspaceSize=128m
  -Dfile.encoding=UTF-8
  -Dlogback.configurationFile=${RIDE_LOGBACK_CONFIG}
  -Dlogback.stdout.level=${RIDE_LOG_LEVEL}
  -Dconfig.override_with_env_vars=true
  -Dwaves.defaults.blockchain.type=$RIDE_NETWORK
  -Dwaves.defaults.directory=$RDATA
  -Dride.heapDumps.enabled=true"

[ -n "${ASYNCPROF_OPTS}" ] && JAVA_OPTS="-agentpath:/usr/local/async-profiler/build/libasyncProfiler.so=$ASYNCPROF_OPTS $JAVA_OPTS"

echo "Ride runner is starting..."
echo "JAVA_OPTS='${JAVA_OPTS}'"

if [ $# -eq 0 ]
  then
    ARGS="$RIDE_APP $RIDE_CONFIG"
  else
    ARGS=$@
fi

java $JAVA_OPTS -cp "${RIDE_INSTALL_PATH}/lib/*" $ARGS
