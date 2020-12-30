#!/bin/bash

if [[ $ENABLE_GRPC == "true" ]]; then
  echo "Installing gRPC server"
  cd /tmp/grpc-server || exit 1
  ls -la
  tar xzf "grpc-server-*.tgz"
  mv /lib/* /usr/share/waves/lib/plugins
fi
