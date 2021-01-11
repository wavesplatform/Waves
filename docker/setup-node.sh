#!/bin/bash

groupadd -g 143 waves
useradd -d /var/lib/waves -g 143 -u 143 -s /bin/bash -M waves
mkdir -p /var/lib/waves /etc/waves /usr/share/waves/lib/plugins
chown -R 143:143 /var/lib/waves /usr/share/waves /etc/waves
chmod -R 755 /var/lib/waves /usr/share/waves /etc/waves
ln -fs /var/lib/waves/log /var/log/waves
chmod +x /usr/share/waves/bin/entrypoint.sh

if [[ $ENABLE_GRPC == "true" ]]; then
  echo "Installing gRPC server"
  cd /tmp/grpc-server || exit 1
  tar xzf grpc-server-*.tgz
  mv /lib/* /usr/share/waves/lib/plugins
fi
