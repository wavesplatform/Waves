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

Scorex - The modular blockchain framework [![Build Status](https://travis-ci.org/wavesplatform/Scorex.svg?branch=master)](https://travis-ci.org/wavesplatform/Scorex)
=========================

[![Join the chat at https://gitter.im/input-output-hk/Scorex](https://badges.gitter.im/input-output-hk/Scorex.svg)](https://gitter.im/input-output-hk/Scorex?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Motivation
----------

There are two huge problems around cryptocurrencies development project Scorex aims to weaken:

* Bitcoin Core source code contains more 100K lines of code(80K of C++ only), Nxt is more than 45K
 line of Java code. All parts of the design(network/transactional/consensus protocols) are mixed in a hard way.
 So researchers and developers are not in good start positions to make experiments.

 In opposite, Scorex is less than 4K lines of Scala code. Transactional layer is as simple as that. Consensus algo
 could be switched easily(with two consensus algos out of the box, one could be replaced with an another with
  just one line of code edited!)

* New coins are trying to make money immediately, often having just one or two pretty controversial
 feature. Scorex is the free and open tool aiming to make other systems development easier.

Features
--------

* Compact, functional code
* Modular design
* Scala language
* Two 100% Proof-of-Stake consensus algos out of the box, Nxt-like and Qora-like. One algo could be replaced
with an another with just one line of code edited.
* Additional consensus module Proof-of-Work consensus algo is available as [separate module](https://github.com/ScorexProject/Permacoin-consensus)
* Simplest transactional model
* Asynchronous network layer on top of TCP
* JSON API
* Command line client for the JSON API
* Cryptographic primitives externalized into [separate scrypto framework](https://github.com/ScorexProject/scrypto)

Getting Started
---------------

* [Quick start guide](https://github.com/ScorexProject/Scorex/wiki/Getting-started)
* [Documentation](https://github.com/ScorexProject/Scorex/wiki)
* [Example project](https://github.com/ScorexProject/Lagonaki)
* [Additional consensus module](https://github.com/ScorexProject/Permacoin-consensus)
* [CI](http://23.94.190.226:8080/job/scorex/)
* [Releases](https://github.com/ScorexProject/Scorex/releases)

Command-Line Client
-------------------

Run ./cli.sh after launching server to issue API requests to it via command-line client. See API section below.
Some examples of CLI commands:

 * GET blocks/first
 * POST payment {"amount":400, "fee":1, "sender":"2kx3DyWJpYYfLErWpRMLHwkL1ZGyKHAPNKr","recipient":"Y2BXLjiAhPUMSo8iBbDEhv81VwKnytTXsH"}

More reading
------------

Besides of [documentation](https://github.com/ScorexProject/Scorex/wiki) there are other resources describing Scorex:

Articles:

[The Architecture Of A Cryptocurrency](docs/articles/components.md)

[On the Way to a Modular Cryptocurrency, Part 1: Generic Block Structure](docs/articles/modular1.md)

[On the Way to a Modular Cryptocurrency, Part 2: Stackable API](docs/articles/modular2.md)

[On Private Blockchains, Technically](docs/artices/private-chains.md)

Readmes:

[Scorex-Basics Sub-Module](scorex-basics/README.md)

Others:

[API Description](docs/API.md)

Please join our mail-list: [https://groups.io/g/scorex-dev](https://groups.io/g/scorex-dev) .


Contributions
-------------

Contributions are welcome! Please take a look into [issues](https://github.com/ConsensusResearch/Scorex-Lagonaki/issues).
 Testing codebase is very small at the moment, so writing a test is not just good for start, but useful as well.

License
-------

To the extent possible under law, the authors have dedicated all copyright and related and neighboring
rights to this software to the public domain worldwide. This software is distributed without any warranty.
You can find applied CC0 license legalcode in the [COPYING](COPYING)
