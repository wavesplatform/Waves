#!/bin/bash
shopt -s nullglob

# Useful options:
#   -XX:+HeapDumpOnOutOfMemoryError
#   -XX:HeapDumpPath=${RDATA}/heap-dumps
#
# -Djdk.attach.allowAttachSelf=true for ehcache/sizeoOf
#
# GCLockerRetryAllocationCount to prevent false OOM
#
# UnlockDiagnosticVMOptions is required for GCLockerRetryAllocationCount, otherwise we get:
#   Error: VM option 'GCLockerRetryAllocationCount' is diagnostic and must be enabled via -XX:+UnlockDiagnosticVMOptions.
#   Error: The unlock option must precede 'GCLockerRetryAllocationCount'.
JAVA_OPTS="-javaagent:${RIDE_INSTALL_PATH}/kanela-agent/kanela-agent-1.0.17.jar
  --add-opens=java.base/java.lang=ALL-UNNAMED
  --add-opens=java.base/java.math=ALL-UNNAMED
  --add-opens=java.base/java.util=ALL-UNNAMED
  -XX:+UnlockDiagnosticVMOptions
  -Xlog:gc::time,level,tags
  -XX:+ExitOnOutOfMemoryError
  -XX:+UseG1GC
  -XX:+ParallelRefProcEnabled
  -XX:+UseStringDeduplication
  -XX:GCLockerRetryAllocationCount=100
  -Xmx${RIDE_HEAP_SIZE}
  -XX:MaxMetaspaceSize=152m
  -Djdk.attach.allowAttachSelf=true
  -Dfile.encoding=UTF-8
  -Dlogback.configurationFile=${RIDE_LOGBACK_CONFIG}
  -Dlogback.stdout.level=${RIDE_LOG_LEVEL}
  -Dlogback.shutdown-hook-delay=2000
  -Dconfig.override_with_env_vars=true
  -Dwaves.defaults.blockchain.type=$RIDE_NETWORK
  -Dwaves.defaults.directory=$RDATA"

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
