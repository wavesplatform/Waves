#!/bin/bash

HOME=/usr/share/waves

groupadd -g 143 waves && useradd -d /var/lib/waves -g 143 -u 143 -s /bin/bash -M waves

mkdir -p /var/lib/waves /etc/waves $HOME/lib/plugins
chown -R 143:143 /var/lib/waves $HOME /etc/waves
chmod -R 755 /var/lib/waves $HOME /etc/waves
ln -fs /var/lib/waves/log /var/log/waves
chmod +x $HOME/bin/entrypoint.sh

if [[ $ENABLE_GRPC == "true" ]]; then
  echo "Installing gRPC server"
  cd /tmp/grpc-server || exit 1
  tar xzf ./grpc-server-*.tgz
  mv ./grpc-server-*/lib/* $HOME/lib/plugins
fi

# Additional files for integration tests
apt-get update && apt-get install -y wget unzip

wget --quiet "https://search.maven.org/remotecontent?filepath=org/aspectj/aspectjweaver/1.9.1/aspectjweaver-1.9.1.jar" -O $HOME/aspectjweaver.jar

YOURKIT_ARCHIVE="YourKit-JavaProfiler-2019.8-docker.zip"
wget --quiet "https://www.yourkit.com/download/docker/$YOURKIT_ARCHIVE" -P /tmp/ &&
  unzip /tmp/$YOURKIT_ARCHIVE -d /usr/local &&
  rm -f /tmp/$YOURKIT_ARCHIVE

# Clean
apt-get remove -y wget unzip && apt-get autoremove -y && rm -rf /var/lib/apt/lists/*
