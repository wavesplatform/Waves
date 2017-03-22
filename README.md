# Waves [![Build Status](https://travis-ci.org/wavesplatform/Waves.svg?branch=master)](https://travis-ci.org/wavesplatform/Waves) [![](https://images.microbadger.com/badges/version/wavesplatform/waves-testnet.svg)](http://microbadger.com/images/wavesplatform/waves-testnet "Testnet Node Docker image")


[How to configure Waves node](https://github.com/wavesplatform/Waves/wiki/How-to-install-Waves-node)

**[How to run Testnet node](https://github.com/wavesplatform/Waves/blob/master/Testnet.md)**


# Installation

Please read [repo wiki article](https://github.com/wavesplatform/Waves/wiki/How-to-install-Waves-node).

## Compiling Packages from source

It is only possible to create deb and fat jar packages.

### Install SBT (Scala Build Tool)

For Ubuntu/Debian:

```
echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
sudo apt-get update
sudo apt-get install sbt
```

You can install sbt on Mac OS X using Homebrew.

### Create Package

Clone this repo and execute

```
sbt packageAll
```

.deb and .jar packages will be in /package folder. To build testnet packages use

```
sbt packageAll -D=network=testnet
```

# Running Integration Tests

## TL;DR

 * Make sure you have [Docker](https://www.docker.com/get-docker) and SBT. 
 * `sbt it:test`
 
## Customizing Tests

By default, `it:test` will do the following: 
* Build a container image with the fat jar and a [template.conf](src/it/resources/template.conf). The newly-built image
  will be registered with the local Docker daemon. This image is built with [sbt-docker](https://github.com/marcuslonnberg/sbt-docker)
  plugin. 
* Run the test suites from `src/it/scala`, passing docker image ID via `docker.imageId` system property.

### Debugging

Integration tests run in a forked JVM. To debug test suite code launched by SBT, you will need to add remote debug 
options to `javaOptions` in `IntegrationTest` configuration:

```scala
javaOptions in IntegrationTest += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005"
```

Debugging a node inside a container is a little more complicated: you will need to modify the `WAVES_OPTS` environment
variable before starting a container.

### Running Tests from IDE

You can run integration test suites from your preferred IDE. The only requirement is to have Docker image pre-built and
have `docker.imageId` system property defined for the run configuration. The easiest way to build an image is to issue
`sbt docker` command. You'll find the image ID in the SBT output:

```
...
[info] Step 5/5 : ENTRYPOINT /opt/waves/start-waves.sh
[info]  ---> Using cache
[info]  ---> e243fa08d496
[info] Successfully built e243fa08d496
[info] Tagging image e243fa08d496 with name: com.wavesplatform/root
[success] Total time: 4 s, completed Mar 22, 2017 12:36:34 PM
```

In this example, `e243fa08d496` is the image ID you need. Make sure to re-build the image whenever the node code (not 
the tests) is changed. If you run the tests from SBT, there's no need to manually rebuild the image, SBT will handle
this automatically.