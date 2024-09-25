#!/bin/bash

JAVA_OPTS="-XX:+ExitOnOutOfMemoryError
  -Xmx${WAVES_HEAP_SIZE}
  --add-opens=java.base/java.util.concurrent.atomic=ALL-UNNAMED
  -Dlogback.stdout.level=${WAVES_LOG_LEVEL}
  -Dlogback.file.directory=${WVLOG}
  -Dwaves.config.directory=/etc/waves
  -Dwaves.defaults.blockchain.type=${WAVES_NETWORK}
  -Dwaves.directory=${WVDATA}
  -Dwaves.rest-api.bind-address=0.0.0.0
  ${JAVA_OPTS}"

echo "JAVA_OPTS=${JAVA_OPTS}" | tee -a ${WVLOG}/waves.log

if [ -n ${WAVES_WALLET_SEED} ] ; then
  JAVA_OPTS="-Dwaves.wallet.seed=${WAVES_WALLET_SEED} ${JAVA_OPTS}"
fi

if [ -n ${WAVES_WALLET_PASSWORD} ] ; then
  JAVA_OPTS="-Dwaves.wallet.password=${WAVES_WALLET_PASSWORD} ${JAVA_OPTS}"
fi

exec java $JAVA_OPTS -cp "${WAVES_INSTALL_PATH}/lib/plugins/*:$WAVES_INSTALL_PATH/lib/*" com.wavesplatform.Application $ARGS
