#!/bin/bash

SBT_VERSION=1.3.8

function downloadRelease() {
  test $WAVES_VERSION = "latest" &&
    WAVES_VERSION=$(curl -fsSL "https://api.github.com/repos/wavesplatform/Waves/releases/latest" | tac | grep -m 1 'tag_name.:' | tr -cd '[0-9\.]') &&
    echo "Using latest version '${WAVES_VERSION}'"

  {
    curl -fsSL "https://api.github.com/repos/wavesplatform/Waves/releases" | tac | grep -q "tag_name.*${WAVES_VERSION}" &&
      echo "GitHub release '${WAVES_VERSION}' found" &&
      echo "Downloading Waves '${WAVES_VERSION}' from GitHub" &&
      mkdir -p /waves/node/target &&
      releaseUrl="https://github.com/wavesplatform/Waves/releases/download" &&
      echo "Downloading jar file" &&
      mkdir -p /out/${network} &&
      curl -fL ${releaseUrl}/v${WAVES_VERSION}/waves-all-${WAVES_VERSION}.jar \
        -o /waves/node/target/${network}/waves-all-${WAVES_VERSION}.jar
  } || {
    echo "Release $WAVES_VERSION not found"
    exit 1
  }
}

function downloadBranch() {
  git clone "https://github.com/wavesplatform/Waves.git" \
    --branch $BRANCH waves && cd waves &&
    git config --global user.name "Sbt Builder" &&
    git config --global user.email "sbt@builder.docker" &&
    git tag -a "v${WAVES_VERSION}" -m "Docker build"
}

function moveReleaseFiles() {
  mkdir -p /out/${network} &&
    mv node/target/waves*.deb /out/${network}/ &&
    cp node/target/waves-all*.jar /out/${network}/ &&
    mv grpc-server/target/universal/grpc-server*.tgz /out/${network}/ &&
    mv grpc-server/target/grpc-server*.deb /out/${network}/
}

# TODO: What is $DEB_PACKAGE_NETWORKS and where its used?
function createSbtBuild() {
  echo "Downloading sbt '${SBT_VERSION}'" &&
    curl -fL -o sbt-$SBT_VERSION.deb \
      https://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb &&
    dpkg -i sbt-$SBT_VERSION.deb && rm sbt-$SBT_VERSION.deb &&
    SBT_OPTS="-Xmx2g -XX:ReservedCodeCacheSize=128m" sbt "node/assembly" &&
    for network in $DEB_PACKAGE_NETWORKS; do
      echo "Building '${network}' package" &&
        SBT_OPTS="-XX:ReservedCodeCacheSize=128m -Xmx2g -Dnetwork=${network}" sbt 'packageAll'
      moveReleaseFiles
    done
}

if [[ $WAVES_VERSION == "branch" ]]; then
  echo "Using branch $BRANCH"
  downloadBranch
  createSbtBuild
elif [[ $WAVES_VERSION == "current" ]]; then
  echo "Using current repo"
  mv waves_current waves
  cd waves || exit 1
  createSbtBuild
else
  echo "Using release: $WAVES_VERSION"
  downloadRelease
fi
