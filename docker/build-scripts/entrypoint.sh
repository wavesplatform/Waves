#!/bin/bash
shopt -s nullglob
NETWORKS="mainnet testnet stagenet"

logEcho() {
  echo $1 | gosu waves tee -a /var/log/waves/waves.log
}

mkdir -p $WVDATA $WVLOG
chmod 700 $WVDATA $WVLOG || :

user="$(id -u)"
if [ "$user" = '0' ]; then
  find $WVDATA \! -user waves -exec chown waves '{}' +
  find $WVLOG \! -user waves -exec chown waves '{}' +
fi

[ -z "${WAVES_CONFIG}" ] && WAVES_CONFIG="/etc/waves/waves.conf"
if [[ ! -f "$WAVES_CONFIG" ]]; then
  logEcho "Custom '$WAVES_CONFIG' not found. Using a default one for '${WAVES_NETWORK,,}' network."
  if [[ $NETWORKS == *"${WAVES_NETWORK,,}"* ]]; then
    touch "$WAVES_CONFIG"
    echo "waves.blockchain.type=${WAVES_NETWORK}" >>$WAVES_CONFIG

    sed -i 's/include "local.conf"//' "$WAVES_CONFIG"
    for f in /etc/waves/ext/*.conf; do
      echo "Adding $f extension config to waves.conf"
      echo "include required(\"$f\")" >>$WAVES_CONFIG
    done
    echo 'include "local.conf"' >>$WAVES_CONFIG
  else
    echo "Network '${WAVES_NETWORK,,}' not found. Exiting."
    exit 1
  fi
else
  echo "Found custom '$WAVES_CONFIG'. Using it."
fi

[ -n "${WAVES_WALLET_PASSWORD}" ] && JAVA_OPTS="${JAVA_OPTS} -Dwaves.wallet.password=${WAVES_WALLET_PASSWORD}"
[ -n "${WAVES_WALLET_SEED}" ] && JAVA_OPTS="${JAVA_OPTS} -Dwaves.wallet.seed=${WAVES_WALLET_SEED}"
JAVA_OPTS="${JAVA_OPTS} -Dwaves.data-directory=$WVDATA/data -Dwaves.directory=$WVDATA"

logEcho "Node is starting..."
logEcho "WAVES_HEAP_SIZE='${WAVES_HEAP_SIZE}'"
logEcho "WAVES_LOG_LEVEL='${WAVES_LOG_LEVEL}'"
logEcho "WAVES_NETWORK='${WAVES_NETWORK}'"
logEcho "WAVES_WALLET_SEED='${WAVES_WALLET_SEED}'"
logEcho "WAVES_WALLET_PASSWORD='${WAVES_WALLET_PASSWORD}'"
logEcho "WAVES_CONFIG='${WAVES_CONFIG}'"
logEcho "JAVA_OPTS='${JAVA_OPTS}'"

JAVA_OPTS="-Dlogback.stdout.level=${WAVES_LOG_LEVEL}
  -XX:+ExitOnOutOfMemoryError
  -Xmx${WAVES_HEAP_SIZE}
  -Dlogback.file.directory=$WVLOG
  -Dconfig.override_with_env_vars=true
  ${JAVA_OPTS}
  -cp '/usr/share/waves/lib/plugins/*:/usr/share/waves/lib/*'" exec gosu waves waves "$WAVES_CONFIG"
