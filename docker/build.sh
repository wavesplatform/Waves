#!/bin/bash

function downloadRelease() {
  test $WAVES_VERSION = "latest" &&
    WAVES_VERSION=$(curl -fsSL "https://api.github.com/repos/wavesplatform/Waves/releases/latest" | tac | grep -m 1 'tag_name.:' | tr -cd '[0-9\.]') &&
    echo "Using latest version '${WAVES_VERSION}'"

  {
    curl -fsSL "https://api.github.com/repos/wavesplatform/Waves/releases" | tac | grep -q "tag_name.*${WAVES_VERSION}" &&
      echo "GitHub release '${WAVES_VERSION}' found" &&
      echo "Downloading Waves '${WAVES_VERSION}' from GitHub" &&
      releaseUrl="https://github.com/wavesplatform/Waves/releases/download" &&
      echo "Downloading jar file" &&
      curl -fL ${releaseUrl}/v${WAVES_VERSION}/waves_${WAVES_VERSION}_all.deb -o /out/waves.deb &&
      curl -fL ${releaseUrl}/v${WAVES_VERSION}/grpc-server_${WAVES_VERSION}_all.deb -o /out/grpc-server.deb
  } || {
    echo "Release $WAVES_VERSION not found"
    exit 1
  }
}

mkdir -p /out

if [[ $WAVES_VERSION == "current" ]]; then
  echo "Using current repo"
  cp /waves_current/* /out/
else
  echo "Using release: $WAVES_VERSION"
  downloadRelease
fi
