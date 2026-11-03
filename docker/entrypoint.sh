#!/bin/bash

# The node itself must not run as root, and only the ownership fix here needs privilege, so this script re-execs
# itself unprivileged: everything below runs as waves, the JVM included. The chown is what makes a bind-mounted
# host directory usable (it arrives with the host's ownership; a fresh named volume inherits the image's instead),
# and it is skipped once the directory is already ours, so a normal start costs one stat. An operator who runs the
# image with --user is left alone.
if [ "$(id -u)" = "0" ] ; then
  for dir in "$WVDATA" "$WVLOG" ; do
    [ "$(stat -c %u "$dir")" = "$(id -u waves)" ] || chown -R waves:waves "$dir"
  done
  exec setpriv --reuid=waves --regid=waves --init-groups --inh-caps=-all --no-new-privs "$0" "$@"
fi

JAVA_OPTS="-XX:+ExitOnOutOfMemoryError
  -Xmx${WAVES_HEAP_SIZE}
  --add-opens=java.base/java.util.concurrent.atomic=ALL-UNNAMED
  --add-opens=java.base/sun.nio.ch=ALL-UNNAMED
  -Dlogback.stdout.level=${WAVES_LOG_LEVEL}
  -Dlogback.file.directory=${WVLOG}
  -Dlogback.file.level=TRACE
  -Dwaves.config.directory=/etc/waves
  -Dwaves.defaults.blockchain.type=${WAVES_NETWORK}
  -Dwaves.directory=${WVDATA}
  -Dwaves.rest-api.bind-address=0.0.0.0
  ${JAVA_OPTS}"

if [ "$WAVES_LOG_JAVA_OPTS" = "true" ] ; then
  echo "JAVA_OPTS=${JAVA_OPTS}" | tee -a ${WVLOG}/waves.log
fi

if [ -n "$WAVES_WALLET_SEED" ] ; then
  JAVA_OPTS="-Dwaves.wallet.seed=${WAVES_WALLET_SEED} ${JAVA_OPTS}"
fi

if [ -n "$WAVES_WALLET_PASSWORD" ] ; then
  JAVA_OPTS="-Dwaves.wallet.password=\"${WAVES_WALLET_PASSWORD}\" ${JAVA_OPTS}"
fi

if [ $# -eq 0 ] && [ -f /etc/waves/waves.conf ] ; then
  ARGS=(/etc/waves/waves.conf)
else
  ARGS=("$@")
fi

# JAVA_OPTS arrives as one string, so plain expansion would word-split a value containing spaces into several
# arguments and java would read the second word as the main class name. xargs re-splits it honoring the quotes
# around such values.
mapfile -t JAVA_ARGS < <(printf '%s' "$JAVA_OPTS" | xargs printf '%s\n')

exec java "${JAVA_ARGS[@]}" -cp "$WAVES_INSTALL_PATH/lib/plugins/*:$WAVES_INSTALL_PATH/lib/*" com.wavesplatform.Application "${ARGS[@]}"
