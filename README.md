# Waves [![Build Status](https://travis-ci.org/wavesplatform/Waves.svg?branch=master)](https://travis-ci.org/wavesplatform/Waves)


[How to configure Waves node](https://github.com/wavesplatform/Waves/wiki/How-to-configure-Waves-node)

**[How to run Testnet node](https://github.com/wavesplatform/Waves/blob/master/Testnet.md)**


# Installation
## Ubuntu

Ubuntu Server

#####Install Oracle Java8 JDK:

`echo "deb http://ppa.launchpad.net/webupd8team/java/ubuntu precise main" | tee -a /etc/apt/sources.list`

`echo "deb-src http://ppa.launchpad.net/webupd8team/java/ubuntu precise main" | tee -a /etc/apt/sources.list`

`apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys EEA14886`

`apt-get update`

`apt-get install oracle-java8-installer`

and agree with license terms



## Run a node from package

### Ubuntu

Download deb package from [releases](https://github.com/wavesplatform/Waves/releases)

Install downloaded package using `sudo dpkg -i` command

Run `waves waves-testnet.json`.


## Compile and Create package from source

For now it is only possible to create deb package.

#####Install SBT (Scala Build Tool):

`echo "deb http://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list`

`apt-get update`

`apt-get install sbt`

#####Create package

`sbt debian:packageBin`

.deb package will be in /target folder

## Other system

Compile code by typing `sbt recompile`

Run a node with `java -jar target/scala-2.11/waves.jar settings.json` command


