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

Built on Scorex
=========================

[![Join the chat at https://gitter.im/input-output-hk/Scorex](https://badges.gitter.im/input-output-hk/Scorex.svg)](https://gitter.im/input-output-hk/Scorex?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Features
--------

* Compact, functional code
* Modular design
* Scala language

License
-------

To the extent possible under law, the authors have dedicated all copyright and related and neighboring
rights to this software to the public domain worldwide. This software is distributed without any warranty.
You can find applied CC0 license legalcode in the [COPYING](COPYING)
