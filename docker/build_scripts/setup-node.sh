#!/bin/bash

# Install DEB packages
dpkg -i /tmp/waves.deb || exit 1

if [[ $ENABLE_GRPC == "true" ]]; then
  echo "Installing gRPC server"
  dpkg -i /tmp/grpc-server.deb || exit 1
fi

rm /etc/waves/waves.conf # Remove example config
cp /tmp/entrypoint.sh /usr/share/waves/bin/entrypoint.sh
chmod +x /usr/share/waves/bin/entrypoint.sh
