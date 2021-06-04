#!/bin/bash

# Create user
groupadd -r waves --gid=999
useradd -r -g waves --uid=999 --home-dir=$WVDATA --shell=/bin/bash waves

# Install DEB packages
dpkg -i /tmp/waves.deb || exit 1
if [[ $ENABLE_GRPC == "true" ]]; then
  echo "Installing gRPC server"
  dpkg -i /tmp/waves-grpc-server.deb || exit 1
fi

# Set permissions
chown -R waves:waves $WVDATA $WVLOG && chmod 777 $WVDATA $WVLOG

rm /etc/waves/waves.conf # Remove example config
cp /tmp/entrypoint.sh /usr/share/waves/bin/entrypoint.sh
chmod +x /usr/share/waves/bin/entrypoint.sh

# Cleanup
rm -rf /tmp/*
