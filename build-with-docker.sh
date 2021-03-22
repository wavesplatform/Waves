#!/bin/bash

NETWORK="Mainnet"
VER_CMD=""

if [[ -n $1 ]]; then
  NETWORK=$1
fi

if [[ -n $2 ]]; then
  echo "Building with version: $2"
  VER_CMD="set ThisBuild/version := \\\"$2\\\""
  echo $VER_CMD
fi

echo "Building with network: $NETWORK"
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
USER=$(id -u)
GROUP=$(id -g)

docker run --mount type=bind,source="$DIR",target=/src -i mozilla/sbt:8u232_1.3.8 /bin/sh -c "
  cd /src &&
  COURSIER_CACHE=\"$DIR/target/docker/coursier\" exec sbt \"-Dsbt.boot.directory=$DIR/target/docker/sbt_cache\" \"set ThisBuild/network := $NETWORK\" \"$VER_CMD\" packageAll &&
  chown -R $USER:$GROUP .
"
