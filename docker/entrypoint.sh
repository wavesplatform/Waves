#!/bin/bash
set -o pipefail

# Root only to take ownership of the data and log directories, then re-exec as waves; --user is left alone.
if [ "$(id -u)" = "0" ] ; then
  for dir in "$WVDATA" "$WVLOG" ; do
    # find stops at the first foreign file, so a tree left root-owned by an older image is fixed on upgrade too
    if [ "$(stat -c %u "$dir")" != "$(id -u waves)" ] || [ -n "$(find "$dir" -xdev ! -user waves -print -quit)" ] ; then
      chown -R waves:waves "$dir"
    fi
  done
  exec setpriv --reuid=waves --regid=waves --init-groups --inh-caps=-all --no-new-privs "$0" "$@"
fi

JAVA_ARGS=(
  -XX:+ExitOnOutOfMemoryError
  "-Xmx${WAVES_HEAP_SIZE}"
  --add-opens=java.base/java.util.concurrent.atomic=ALL-UNNAMED
  --add-opens=java.base/sun.nio.ch=ALL-UNNAMED
  "-Dlogback.stdout.level=${WAVES_LOG_LEVEL}"
  "-Dlogback.file.directory=${WVLOG}"
  -Dlogback.file.level=TRACE
  -Dwaves.config.directory=/etc/waves
  "-Dwaves.defaults.blockchain.type=${WAVES_NETWORK}"
  "-Dwaves.directory=${WVDATA}"
  -Dwaves.rest-api.bind-address=0.0.0.0
)

# The only line that names the variable in braces: node-it's restartNode appends options to it with sed, quotes
# included, which a heredoc keeps literal.
USER_JAVA_OPTS=$(cat <<EOF
${JAVA_OPTS}
EOF
)
# Only the operator's options are re-split, honouring quotes around values with spaces; a parse error stops the start.
USER_OPTS=$(printf '%s' "$USER_JAVA_OPTS" | xargs -r printf '%s\n') || { echo "Cannot parse JAVA_OPTS" >&2; exit 1; }
[ -n "$USER_OPTS" ] && mapfile -t -O "${#JAVA_ARGS[@]}" JAVA_ARGS <<< "$USER_OPTS"

if [ "$WAVES_LOG_JAVA_OPTS" = "true" ] ; then
  echo "JAVA_OPTS=${JAVA_ARGS[*]}" | tee -a "${WVLOG}/waves.log"
fi

# Secrets are added after logging and never go through xargs, so any character in them is passed as is
[ -n "$WAVES_WALLET_SEED" ] && JAVA_ARGS=("-Dwaves.wallet.seed=${WAVES_WALLET_SEED}" "${JAVA_ARGS[@]}")
[ -n "$WAVES_WALLET_PASSWORD" ] && JAVA_ARGS=("-Dwaves.wallet.password=${WAVES_WALLET_PASSWORD}" "${JAVA_ARGS[@]}")

if [ $# -eq 0 ] && [ -f /etc/waves/waves.conf ] ; then
  ARGS=(/etc/waves/waves.conf)
else
  ARGS=("$@")
fi

exec java "${JAVA_ARGS[@]}" -cp "$WAVES_INSTALL_PATH/lib/plugins/*:$WAVES_INSTALL_PATH/lib/*" com.wavesplatform.Application "${ARGS[@]}"
