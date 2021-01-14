#!/bin/bash

HOME=/usr/share/waves
chmod +x $HOME/bin/entrypoint.sh

# Additional dependencies
mkdir -p /usr/share/man/man1
apt-get update && apt-get install -y wget unzip default-jre-headless || exit 1

# Install DEB packages
dpkg -i /tmp/waves.deb || exit 1

if [[ $ENABLE_GRPC == "true" ]]; then
  echo "Installing gRPC server"
 dpkg -i /tmp/grpc-server.deb || exit 1
fi

rm /etc/waves/waves.conf # Remove example config

wget --quiet "https://search.maven.org/remotecontent?filepath=org/aspectj/aspectjweaver/1.9.1/aspectjweaver-1.9.1.jar" -O $HOME/aspectjweaver.jar

YOURKIT_ARCHIVE="YourKit-JavaProfiler-2019.8-docker.zip"
wget --quiet "https://www.yourkit.com/download/docker/$YOURKIT_ARCHIVE" -P /tmp/ &&
  unzip /tmp/$YOURKIT_ARCHIVE -d /usr/local &&
  rm -f /tmp/$YOURKIT_ARCHIVE

# Clean
apt-get remove -y wget unzip && apt-get autoremove -y && apt-get autoclean && rm -rf /var/lib/apt/lists/*
rm -rf /tmp/*
