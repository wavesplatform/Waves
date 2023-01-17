#!/bin/bash
shopt -s nullglob

[ -n "${YOURKIT_OPTS}" ] && JAVA_OPTS="$JAVA_OPTS -agentpath:/usr/local/YourKit-JavaProfiler-$YOURKIT_VERSION/bin/linux-x86-64/libyjpagent.so=$YOURKIT_OPTS"
JAVA_OPTS="-javaagent:${RIDE_INSTALL_PATH}/kanela-agent/kanela-agent-1.0.16.jar
  -server
  -XX:+ExitOnOutOfMemoryError
  -XX:+ParallelRefProcEnabled
  -XX:+UseStringDeduplication
  -Xmx${RIDE_HEAP_SIZE}
  -Dfile.encoding=UTF-8
  -Dlogback.configurationFile=${RIDE_LOGBACK_CONFIG}
  -Dlogback.stdout.level=${RIDE_LOG_LEVEL}
  -Dconfig.override_with_env_vars=true
  ${JAVA_OPTS}
  -Dwaves.defaults.blockchain.type=$RIDE_NETWORK
  -Dwaves.defaults.directory=$RDATA"

echo "Ride runner is starting..."
echo "JAVA_OPTS='${JAVA_OPTS}'"

java $JAVA_OPTS -cp "${RIDE_INSTALL_PATH}/lib/*" $RIDE_APP "$RIDE_CONFIG"
