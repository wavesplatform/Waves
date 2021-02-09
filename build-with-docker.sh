#!/bin/bash

docker run --mount type=bind,source="$(pwd)",target=/src -it mozilla/sbt:8u232_1.3.8 /bin/sh -c '
  cd /src &&
  COURSIER_CACHE=$(pwd)/docker/temp/coursier sbt "-Dsbt.boot.directory=$(pwd)/docker/temp/sbt_cache" packageAll
'
