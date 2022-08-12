#!/bin/bash

NETWORKS="mainnet testnet stagenet custom"

# Create data directories
mkdir -p $WVDATA $WVLOG /etc/waves

# Create user
groupadd -r waves --gid=999
useradd -r -g waves --uid=999 --home-dir=$WVDATA --shell=/bin/bash waves

# Unpack tgz packages
mkdir -p $WAVES_INSTALL_PATH
tar zxvf /tmp/waves.tgz -C $WAVES_INSTALL_PATH --strip-components=1
if [[ $ENABLE_GRPC == true ]]; then
  echo "Unpacking gRPC server"
  tar zxvf /tmp/waves-grpc-server.tgz -C $WAVES_INSTALL_PATH --strip-components=1
fi

# Set permissions
chown -R waves:waves $WVDATA $WVLOG $WAVES_INSTALL_PATH && chmod 755 $WVDATA $WVLOG

cp /tmp/entrypoint.sh $WAVES_INSTALL_PATH/bin/entrypoint.sh
chmod +x $WAVES_INSTALL_PATH/bin/entrypoint.sh

if [[ $NETWORKS == *"${WAVES_NETWORK,,}"* ]]; then
  # don't use indentation for heredoc because of its restrictions
eval "cat <<EOF
$(</tmp/waves.conf.template)
EOF" 2> /dev/null > $WAVES_CONFIG

else
  echo "Network '${WAVES_NETWORK,,}' not found. Exiting."
  exit 1
fi

cp $WAVES_INSTALL_PATH/lib/plugins/* $WAVES_INSTALL_PATH/lib/

# Cleanup
rm -rf /tmp/*
