<h1 align="center">🔷 Waves Node</h1>

<p align="center">
  <a href="https://github.com/wavesplatform/Waves/actions" target="_blank">
    <img alt="Checks status" src="https://badgen.net/github/checks/wavesplatform/waves?cache=600"  />
  </a>
  <a href="https://github.com/wavesplatform/Waves/releases" target="_blank">
    <img alt="Downloads" src="https://badgen.net/github/assets-dl/wavesplatform/waves?color=blue" />
  </a>
  <a href="https://hub.docker.com/r/wavesplatform/wavesnode" target="_blank">
    <img alt="Docker pulls" src="https://badgen.net/docker/pulls/wavesplatform/wavesnode?icon=docker" />
  </a>

  <br/>

  <a href="https://twitter.com/wavesprotocol" target="_blank">
    <img alt="Twitter: Waves Tech" src="https://badgen.net/twitter/follow/wavesprotocol?icon=twitter&label=follow%20on%20Twitter" />
  </a>
  <a href="https://medium.com/wavesprotocol" target="_blank">
    <img alt="Medium: Waves Tech" src="https://badgen.net/runkit/msmolyakov/get-medium-followers?icon=medium&cache=86400" />
  </a>
  <a href="https://t.me/waves_ride_dapps_dev" target="_blank">
    <img alt="Telegram" src="https://badgen.net/badge/icon/Waves%20Dev%20Jedi?icon=telegram&label=Telegram"/>
  </a>
  <a href="https://github.com/msmolyakov/awesome-waves" target="_blank">
    <img alt="Awesome Waves" src="https://badgen.net/badge/icon/Awesome%20Waves?icon=awesome&label&color=pink" />
  </a>
</p>

> Waves is an open source [blockchain protocol](https://waves.tech/waves-protocol). <br/> 
You can use it to build your own decentralized applications. Waves provides full blockchain ecosystem including smart contracts language called RIDE.


## ✨ Demo

<p align="center">
    <img src="https://user-images.githubusercontent.com/1945126/78667964-88209480-78e2-11ea-9304-72178a6a5974.gif" alt="Waves Node Run Demo">
</p>

Waves node is a host connected to the blockchain network with the following functions:

- Processing and validation of [transactions](https://docs.waves.tech/en/blockchain/transaction/transaction-validation)
- Generation and storage of [blocks](https://docs.waves.tech/en/blockchain/block/)
- Network communication with [other nodes](https://docs.waves.tech/en/blockchain/blockchain/#node)
- [REST API](https://docs.waves.tech/en/waves-node/node-api/)
- [Extensions](https://docs.waves.tech/en/waves-node/extensions/) management

Learn more about Waves Node in the [documentation](https://docs.waves.tech/en/waves-node/).

## 🚀️ Getting started

A quick introduction of the minimal setup you need to get a running node. 

*Prerequisites:*
- configuration file for a needed network from [here](https://github.com/wavesplatform/Waves/tree/HEAD/node)
- `waves-all*.jar` file from [releases](https://github.com/wavesplatform/Waves/releases) 

Linux systems:
```bash
sudo apt-get update
sudo apt-get install openjdk-11-jre
java -jar node/target/waves-all*.jar path/to/config/waves-{network}.conf
```

Mac systems (assuming already installed homebrew):
```bash
brew cask install adoptopenjdk/openjdk/adoptopenjdk11
java -jar node/target/waves-all*.jar path/to/config/waves-{network}.conf
```

Windows systems (assuming already installed OpenJDK 11):
```bash
java -jar node/target/waves-all*.jar path/to/config/waves-{network}.conf
```

Using docker, follow the [official image documentation](https://hub.docker.com/r/wavesplatform/wavesnode).

> More details on how to install a node for different platforms you can [find in the documentation](https://docs.waves.tech/en/waves-node/how-to-install-a-node/how-to-install-a-node). 

## 🔧 Configuration

The best starting point to understand available configuration parameters is [this article](https://docs.waves.tech/en/waves-node/node-configuration).

The easiest way to start playing around with configurations is to use default configuration files for different networks; they're available in [network-defaults.conf](./node/src/main/resources/network-defaults.conf).

Logging configuration with all available levels and parameters is described [here](https://docs.waves.tech/en/waves-node/logging-configuration).

## 👨‍💻 Development

The node can be built and installed wherever Java can run. 
To build and test this project, you will have to follow these steps:

<details><summary><b>Show instructions</b></summary>

*1. Setup the environment.*
- Install Java for your platform:

```bash
sudo apt-get update
sudo apt-get install openjdk-11-jre                     # Ubuntu
# or
# brew cask install adoptopenjdk/openjdk/adoptopenjdk11 # Mac
```

- Install SBT (Scala Build Tool)

Please follow the SBT installation instructions depending on your platform ([Linux](https://www.scala-sbt.org/1.0/docs/Installing-sbt-on-Linux.html), [Mac](https://www.scala-sbt.org/1.0/docs/Installing-sbt-on-Mac.html), [Windows](https://www.scala-sbt.org/1.0/docs/Installing-sbt-on-Windows.html))

*2. Clone this repo*

```bash
git clone https://github.com/wavesplatform/Waves.git
cd Waves
```

*3. Compile and run tests*

```bash
sbt checkPR
```

*4. Run integration tests (optional)*

Create a Docker image before you run any test: 
```bash
sbt node-it/docker
```

- Run all tests. You can increase or decrease number of parallel running tests by changing `waves.it.max-parallel-suites`
system property:
```bash
sbt -Dwaves.it.max-parallel-suites=1 node-it/test
```

- Run one test:
```bash
sbt node-it/testOnly *.TestClassName
# or 
# bash node-it/testOnly full.package.TestClassName
```

*5. Build packages* 

```bash
sbt packageAll                   # Mainnet
sbt -Dnetwork=testnet packageAll # Testnet
```

`sbt packageAll` ‌produces only `deb` package along with a fat `jar`. 

*6. Install DEB package*

`deb` package is located in target folder. You can replace '*' with actual package name:

```bash
sudo dpkg -i node/target/*.deb
```


*7. Run an extension project locally during development (optional)*

```bash
sbt "extension-module/run /path/to/configuration"
```

*8. Configure IntelliJ IDEA (optional)*

The majority of contributors to this project use IntelliJ IDEA for development, if you want to use it as well please follow these steps:

1. Click `Add configuration` (or `Edit configurations...`).
2. Click `+` to add a new configuration, choose `Application`.
3. Specify:
   - Main class: `com.wavesplatform.Application`
   - Program arguments: `/path/to/configuration`
   - Use classpath of module: `extension-module`
4. Click `OK`.
5. Run this configuration.

</details>

## 🤝 Contributing

If you'd like to contribute, please fork the repository and use a feature branch. Pull requests are warmly welcome.

For major changes, please open an issue first to discuss what you would like to change. Please make sure to update tests as appropriate.

Please follow the [code of conduct](./CODE_OF_CONDUCT.md) during communication with each other. 

## ℹ️ Support (get help)

Keep up with the latest news and articles, and find out all about events happening on the [Waves Protocol](https://waves.tech/).

- [Telegram Dev Chat](https://t.me/waves_ride_dapps_dev)
- [Waves Blog](https://medium.com/wavesprotocol)

## ⛓ Links

- [Documentation](https://docs.waves.tech/)
- Blockchain clients for Mainnet: [Waves Exchange](https://waves.exchange/), [Waves FX](https://github.com/wavesfx), [SIGN app](https://www.sign-web.app/)
- Blockchain clients for Testnet: [Waves Exchange](https://testnet.waves.exchange/)
- Blockchain Explorer: [Mainnet](https://wavesexplorer.com/), [Testnet](https://testnet.wavesexplorer.com/), [Stagenet](https://stagenet.wavesexplorer.com/) 
- [Ride Online IDE](https://waves-ide.com/)

## 📝 Licence

The code in this project is licensed under [MIT license](./LICENSE)

## 👏 Acknowledgements

[<img src="https://camo.githubusercontent.com/97fa03cac759a772255b93c64ab1c9f76a103681/68747470733a2f2f7777772e796f75726b69742e636f6d2f696d616765732f796b6c6f676f2e706e67">](https://www.yourkit.com/)

We use YourKit full-featured Java Profiler to make Waves node faster. YourKit, LLC is the creator of innovative and intelligent tools for profiling Java and .NET applications.

Take a look at YourKit's leading software products: YourKit Java Profiler and YourKit .NET Profiler.
