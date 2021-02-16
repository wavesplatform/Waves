#!/bin/bash

NETWORK=Mainnet
if [[ -n $1 ]]; then
  echo "Building with custom network: $1"
  NETWORK=$1
fi

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

docker run --mount type=bind,source="$DIR",target=/src -i mozilla/sbt:8u232_1.3.8 /bin/sh -c "
  cd /src &&
  COURSIER_CACHE=\"$DIR/target/docker/coursier\" sbt \"-Dsbt.boot.directory=$DIR/target/docker/sbt_cache\" \"set ThisBuild/network := $1\" packageAll
"
