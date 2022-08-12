#!/bin/bash
shopt -s nullglob

logEcho() {
  echo $1 | gosu waves tee -a /var/log/waves/waves.log
}

[ -n "${WAVES_WALLET_PASSWORD}" ] && JAVA_OPTS="${JAVA_OPTS} -Dwaves.wallet.password=${WAVES_WALLET_PASSWORD}"
[ -n "${WAVES_WALLET_SEED}" ] && JAVA_OPTS="${JAVA_OPTS} -Dwaves.wallet.seed=${WAVES_WALLET_SEED}"
JAVA_OPTS="${JAVA_OPTS} -Dwaves.directory=$WVDATA"

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

exec gosu waves $WAVES_INSTALL_PATH/bin/waves "$WAVES_CONFIG"

