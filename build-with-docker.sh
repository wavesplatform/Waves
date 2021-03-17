#!/bin/bash

NETWORK="Mainnet"

if [[ -n $1 ]]; then
  NETWORK=$1
fi

echo "Building with network: $NETWORK"
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
USER=$(id -u)
GROUP=$(id -g)

docker run --mount type=bind,source="$DIR",target=/src -i mozilla/sbt:8u232_1.3.8 /bin/sh -c "
  cd /src &&
  COURSIER_CACHE=\"$DIR/target/docker/coursier\" sbt \"-Dsbt.boot.directory=$DIR/target/docker/sbt_cache\" \"set ThisBuild/network := $NETWORK\" packageAll &&
  chown -R $USER:$GROUP .
"
