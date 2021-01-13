#!/bin/bash
shopt -s nullglob
NETWORKS="mainnet testnet stagenet"

mkdir -p /var/lib/waves/log

[ -z "${WAVES_CONFIG}" ] && WAVES_CONFIG="/etc/waves/waves.conf"
if [[ ! -f "$WAVES_CONFIG" ]]; then
  echo "Custom '$WAVES_CONFIG' not found. Using a default one for '${WAVES_NETWORK,,}' network." | tee -a /var/log/waves/waves.log
  if [[ $NETWORKS == *"${WAVES_NETWORK,,}"* ]]; then
    touch "WAVES_CONFIG"
    echo "waves.blockchain.type=${WAVES_NETWORK}" >> $WAVES_CONFIG

    sed -i 's/include "local.conf"//' "$WAVES_CONFIG"
    for f in /etc/waves/ext/*.conf; do
      echo "Adding $f extension config to waves.conf"
      echo "include required(\"$f\")" >> $WAVES_CONFIG
    done
    echo 'include "local.conf"' >> $WAVES_CONFIG
  else
    echo "Network '${WAVES_NETWORK,,}' not found. Exiting."
    exit 1
  fi
else
  echo "Found custom '$WAVES_CONFIG'. Using it."
fi

if [[ "${WAVES_VERSION}" == "latest" || "${WAVES_VERSION}" == "current" || "${WAVES_VERSION}" == "branch" ]]; then
  filename=$(find /usr/share/waves/lib -name 'waves-all*' -printf '%f\n')
  export WAVES_VERSION=$(echo ${filename##*-} | cut -d\. -f1-3)
fi

[ -n "${WAVES_WALLET_PASSWORD}" ] && JAVA_OPTS="${JAVA_OPTS} -Dwaves.wallet.password=${WAVES_WALLET_PASSWORD}"
[ -n "${WAVES_WALLET_SEED}" ] && JAVA_OPTS="${JAVA_OPTS} -Dwaves.wallet.seed=${WAVES_WALLET_SEED}"
JAVA_OPTS="${JAVA_OPTS} -Dwaves.data-directory=/var/lib/waves/data -Dwaves.directory=/var/lib/waves"

echo "Node is starting..." | tee -a /var/log/waves/waves.log
echo "WAVES_HEAP_SIZE='${WAVES_HEAP_SIZE}'" | tee -a /var/log/waves/waves.log
echo "WAVES_LOG_LEVEL='${WAVES_LOG_LEVEL}'" | tee -a /var/log/waves/waves.log
echo "WAVES_VERSION='${WAVES_VERSION}'" | tee -a /var/log/waves/waves.log
echo "WAVES_NETWORK='${WAVES_NETWORK}'" | tee -a /var/log/waves/waves.log
echo "WAVES_WALLET_SEED='${WAVES_WALLET_SEED}'" | tee -a /var/log/waves/waves.log
echo "WAVES_WALLET_PASSWORD='${WAVES_WALLET_PASSWORD}'" | tee -a /var/log/waves/waves.log
echo "WAVES_CONFIG='${WAVES_CONFIG}'" | tee -a /var/log/waves/waves.log
echo "JAVA_OPTS='${JAVA_OPTS}'" | tee -a /var/log/waves/waves.log

exec java -Dlogback.stdout.level=${WAVES_LOG_LEVEL} \
  -XX:+ExitOnOutOfMemoryError \
  -Xmx${WAVES_HEAP_SIZE} \
  -Dlogback.file.directory=/var/log/waves \
  -Dconfig.override_with_env_vars=true \
  ${JAVA_OPTS} \
  -cp '/usr/share/waves/lib/plugins/*:/usr/share/waves/lib/*' \
  com.wavesplatform.Application \
  "$WAVES_CONFIG"
