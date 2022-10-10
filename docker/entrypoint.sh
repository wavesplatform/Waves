#!/bin/bash
shopt -s nullglob

logEcho() {
  echo $1 | gosu waves tee -a /var/log/waves/waves.log
}
[ -n "${YOURKIT_OPTS}" ] && JAVA_OPTS="$JAVA_OPTS -agentpath:/usr/local/YourKit-JavaProfiler-$YOURKIT_VERSION/bin/linux-x86-64/libyjpagent.so=$YOURKIT_OPTS"
JAVA_OPTS="${JAVA_OPTS} -Dwaves.defaults.blockchain.type=$WAVES_NETWORK -Dwaves.defaults.directory=$WVDATA"

logEcho "Node is starting..."
logEcho "WAVES_HEAP_SIZE='${WAVES_HEAP_SIZE}'"
logEcho "WAVES_LOG_LEVEL='${WAVES_LOG_LEVEL}'"
logEcho "JAVA_OPTS='${JAVA_OPTS}'"

JAVA_OPTS="-Dlogback.stdout.level=${WAVES_LOG_LEVEL}
  -XX:+ExitOnOutOfMemoryError
  -Xmx${WAVES_HEAP_SIZE}
  -Dlogback.file.directory=$WVLOG
  -Dconfig.override_with_env_vars=true
  ${JAVA_OPTS}"

java $JAVA_OPTS -cp "${WAVES_INSTALL_PATH}/lib/plugins/*:$WAVES_INSTALL_PATH/lib/*" com.wavesplatform.Application "$WAVES_CONFIG"
