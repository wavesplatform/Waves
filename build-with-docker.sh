#!/bin/bash

docker run --mount type=bind,source="$(pwd)",target=/src -it mozilla/sbt:8u232_1.3.8 /bin/sh -c '
  cd /src &&
  COURSIER_CACHE=$(pwd)/target/docker/coursier sbt "-Dsbt.boot.directory=$(pwd)/target/docker/sbt_cache" packageAll
'
