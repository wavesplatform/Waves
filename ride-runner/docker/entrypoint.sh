#!/bin/bash
shopt -s nullglob

# Useful options:
#   -XX:+HeapDumpOnOutOfMemoryError
#   -XX:HeapDumpPath=${RDATA}/heap-dumps
# Why are these options required? See ride-runner/build.sbt
JAVA_OPTS="-javaagent:${RIDE_INSTALL_PATH}/kanela-agent/kanela-agent-1.0.18.jar
  --add-opens=java.base/java.lang=ALL-UNNAMED
  --add-opens=java.base/java.math=ALL-UNNAMED
  --add-opens=java.base/java.util=ALL-UNNAMED
  ${JAVA_OPTS}
  -server
  -Xmx${RIDE_HEAP_SIZE}
  -XX:+ExitOnOutOfMemoryError
  -XX:+UseG1GC
  -XX:+ParallelRefProcEnabled
  -XX:+UseStringDeduplication
  -XX:+UnlockDiagnosticVMOptions
  -XX:GCLockerRetryAllocationCount=100
  -XX:MaxMetaspaceSize=152m
  -XX:ThreadStackSize=1024
  -Djdk.attach.allowAttachSelf=true
  -Dfile.encoding=UTF-8
  -Dlogback.configurationFile=${RIDE_LOGBACK_CONFIG}
  -Dlogback.stdout.level=${RIDE_LOG_LEVEL}
  -Dlogback.shutdown-hook-delay=2000
  -Dconfig.override_with_env_vars=true
  -Dwaves.defaults.blockchain.type=$RIDE_NETWORK
  -Dwaves.defaults.directory=$RDATA"

if [[ -n "${ASYNCPROF_ENABLE}" && "${ASYNCPROF_ENABLE}" == "true" ]]; then
  echo "async-profiler enabled"
  JAVA_OPTS="-agentpath:/usr/local/async-profiler/build/libasyncProfiler.so=$ASYNCPROF_OPTS $JAVA_OPTS"
fi

echo "Ride runner is starting..."
echo "JAVA_OPTS='${JAVA_OPTS}'"

if [ $# -eq 0 ]; then
  ARGS="$RIDE_APP $RIDE_CONFIG"
else
  ARGS=$@
fi

if [[ -n "${JEMALLOC_ENABLE}" && "${JEMALLOC_ENABLE}" == "true" ]]; then
  # jemalloc settings
  echo "jemalloc enabled"
  mkdir -p ${RDATA}/jemalloc
  export MALLOC_CONF="${MALLOC_CONF},prof_prefix:${RDATA}/jemalloc/jeprof.out"
  export LD_PRELOAD="/usr/lib/x86_64-linux-gnu/libjemalloc.so"
fi

java $JAVA_OPTS -cp "${RIDE_INSTALL_PATH}/lib/*" $ARGS
