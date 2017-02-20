# Waves [![Build Status](https://travis-ci.org/wavesplatform/Waves.svg?branch=master)](https://travis-ci.org/wavesplatform/Waves) [![](https://images.microbadger.com/badges/version/wavesplatform/waves-testnet.svg)](http://microbadger.com/images/wavesplatform/waves-testnet "Testnet Node Docker image")


[How to configure Waves node](https://github.com/wavesplatform/Waves/wiki/How-to-install-Waves-node)

**[How to run Testnet node](https://github.com/wavesplatform/Waves/blob/master/Testnet.md)**


# Installation

Please read [repo wiki article](https://github.com/wavesplatform/Waves/wiki/How-to-install-Waves-node).

## Compilation the packages from source

It is only possible to create deb and fat jar packages.

#####Install SBT (Scala Build Tool):

For Ubuntu/Debian:

`echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list`

`sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823`

`sudo apt-get update`

`sudo apt-get install sbt`

You can install sbt on Mac OS X using Homebrew.

#####Create package

Clone this repo and execute

`sbt packageAll`

.deb and .jar packages will be in /package folder. To build testnet packages use

`sbt packageAll -D=network=testnet`
