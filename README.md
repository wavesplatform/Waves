# Waves [![Build Status](https://travis-ci.org/wavesplatform/Waves.svg?branch=master)](https://travis-ci.org/wavesplatform/Waves)

Waves is an open source [blockchain platform](https://wavesplatform.com/).

You can use it to build your own decentralised applications. Waves provides full blockchain ecosystem including smart contracts language called RIDE.

### How does the blockchain network work?

There is a huge collection of nodes deployed by miners that store all of the network information in the chain of blocks (blockchain), process requests and can add new transactions to the network after checking their compliance with the rules. The miners are rewarded with the network coins called Waves. 

The main advantage of this technology is that each node is a synchronized copy of the main blockchain: it means that the information is stored decentralized and won't be overwritten globally if one of the users changes it at one of the node storages. This can guarantee that the user's information will stay fair and unchangeable. 



# What is node?

A **node** is a [host](https://en.wikipedia.org/wiki/Host_(network)) connected to the [blockchain network](https://docs.wavesplatform.com/en/blockchain/blockchain-network.html) using the [Waves Node](https://github.com/wavesplatform/Waves) application.

Node functions are:

- [blocks](https://docs.wavesplatform.com/en/blockchain/block.html) storage
- [transaction validation](https://docs.wavesplatform.com/en/blockchain/transaction/transaction-validation.html)
- sending [transactions](https://docs.wavesplatform.com/en/blockchain/transaction.html)



Learn more about Waves Full Node in the [documentation](https://docs.wavesplatform.com/en/waves-node/what-is-a-full-node.html).





# How to Build and Test a Node

The node can be built and installed wherever java can run. For ***Ubuntu***,sbt packageAll ‌produces only deb package but for other operating systems, ZIP archive or a fat JAR can be used as well. To build and test your Waves Node, you will need to follow these steps:



## 1. Setup the environment

- ### Installing Java

```
sudo apt-get update
sudo apt-get install default-jre default-jdk
```

- ### Installing SBT

Please follow the SBT installation instructions depending on your operating system ([Mac](https://www.scala-sbt.org/1.0/docs/Installing-sbt-on-Mac.html), [Windows](https://www.scala-sbt.org/1.0/docs/Installing-sbt-on-Windows.html), [Linux](https://www.scala-sbt.org/1.0/docs/Installing-sbt-on-Linux.html)).



## 2. Obtaining Source Codes

Cloning with HTTPS URLs (recommended)

```
git clone https://github.com/wavesplatform/Waves.git
cd Waves
```

or cloning with SSH URLs

```
git clone git@github.com:wavesplatform/Waves.git
cd Waves
```



## 3. Compilation and unit tests

```
sbt checkPR
```



## 4. Running NODE integration tests (optional)

Create a Docker image before you run any test: `sbt node-it/docker`

- Run all tests: `SBT_THREAD_NUMBER=4 sbt node-it/test` . You can increase or decrease number of parallel running tests by changing `SBT_THREAD_NUMBER`
- Run one test: `sbt node-it/testOnly *.TestClassName` or `node-it/testOnly full.package.TestClassName`



## 5. Building packages

- ### Mainnet

```
sbt packageAll
```

- ### Testnet

```
sbt -Dnetwork=testnet packageAll
```



## 6. Installing DEB package

DEB package located in target folder. You can replace '*' with actual package name:

```
sudo dpkg -i node/target/*.deb
```



## 7. Running fat jar

You can replace waves-all*.jar with actual jar name (it should have "all"-word):

```
java -jar node/target/waves-all*.jar path/to/config/file
```

**Note.** For OSX - homebrew is preferable choice. You can install java with brew cask install java and sbt with brew install sbt@1. Build/Test steps are common for any OS (but you should use ‘' instead of '/' in windows). {% endprettyhint %}



## 8. Running an extension project locally during development

### SBT

```
sbt "extension-module/run /path/to/configuration"

```

### IntelliJ IDEA

1. Click on `Add configuration` (or `Edit configurations...`)
2. Click on `+` to add a new configuration, choose `Application`
3. Specify:
   - Main class: `com.wavesplatform.Application`
   - Program arguments: `/path/to/configuration`
   - Use classpath of module: `extension-module`
4. Click on `OK`
5. Run this configuration



> There is another way to run a Waves Node: using **Waves Docker container**. Learn more about it in the [docs](https://docs.wavesplatform.com/en/waves-node/waves-node-in-docker.html). 



# Useful links

- [Official Documentation](https://docs.wavesplatform.com/)
- [Client Mainnet](https://client.wavesplatform.com/) – Waves Platform client
- [Explorer Mainnet](https://wavesexplorer.com/) – Waves Platform transactions explorer
- [Testnet](https://testnet.wavesplatform.com/) – the alternative Waves blockchain being used for testing
- [Testnet Explorer](https://wavesexplorer.com/testnet) – Test Net transactions explorer
- [RIDE](https://github.com/wavesplatform/waves-documentation/blob/master/en/ride/ride-script.md) – Waves smart contract coding language
- [Waves Ride IDE](https://ide.wavesplatform.com/) – software for RIDE coding



# Support

Keep up with the latest news and articles, and find out all about events happening on the [Waves Platform](https://wavesplatform.com/).

- [Telegram Dev Chat](https://t.me/waves_ride_dapps_dev)
- [Community Forum](https://forum.wavesplatform.com/)
- [Community Portal](https://wavescommunity.com/)
- [Waves Blog](https://blog.wavesplatform.com/)
- [Support](https://support.wavesplatform.com/)



# Acknowledgement
[<img src="https://camo.githubusercontent.com/97fa03cac759a772255b93c64ab1c9f76a103681/68747470733a2f2f7777772e796f75726b69742e636f6d2f696d616765732f796b6c6f676f2e706e67">](https://www.yourkit.com/)

We use YourKit full-featured Java Profiler to make Waves node faster. YourKit, LLC is the creator of innovative and intelligent tools for profiling Java and .NET applications.

Take a look at YourKit's leading software products: YourKit Java Profiler and YourKit .NET Profiler.
