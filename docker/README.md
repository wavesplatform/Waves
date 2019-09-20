# Waves Node in Docker

## About Waves
Waves is a decentralized platform that allows any user to issue, transfer, swap and trade custom blockchain tokens on an integrated peer-to-peer exchange. You can find more information about Waves at [wavesplatform.com](https://wavesplatform.com) and in the official [documentation]((https://docs.wavesplatform.com)).


## About the image
This Docker image contains scripts and configs to run Waves Node for `mainnet`, 'testnet' or 'stagenet' networks.
The image is focused on fast and convenient deployment of Waves Node.

## Prerequisites
It is highly recommended to read more about [Waves Node configuration](https://docs.wavesplatform.com/en/waves-Node/Node-configuration.html) before running the container.

## Building Docker image

Dockerfile supports 3 main scenarios:
1. Basic scenario `docker build .` - build an image with the latest Waves Node release available
*Note*: pre-releases are skipped
2. Existing Version scenario `docker build --build-arg WAVES_VERSION=1.1.1` - specify the version of Waves Node available in GitHub Releases. If this version does not exist, this is the next scenario.
3. Build scenario `docker build --build-arg WAVES_VERSION=99.99.99 --build-arg BRANCH=version-0.17.x` - this scenario assumes that you want to build Waves Node from sources. Use `WAVES_VERSION` build argument to specify a Git tag ('v' is added automatically) and `BRANCH` to specify a Git branch to checkout to. Make sure you specify a tag that does not exist in the repo, otherwise it is the previous scenario.

**You can specify following aarguments when building the inage:**


|Argument              | Default value |Description   |
|----------------------|-------------------|--------------|
|`WAVES_NETWORK`       | `mainnet`         | Waves Blockchain network. Available values are `mainnet`, `testnet`, `stagenet`. Can be overridden in a runtime using environment variable with the same name.|
|`WAVES_VERSION`       | `latest`            | A node version which corresponds to the Git tag we want to use/create. |
|`BRANCH`              | `version-0.17.x`    | Relevant if Git tag 'v`WAVES_VERSION`' does not exist in the public repository. This option represents a Git branch we will use to compile Waves node and set a Git tag on.|
|`SBT_VERSION`         | `1.2.8` 	       | Scala build tool version.|
|`WAVES_LOG_LEVEL`     | `DEBUG`           | Default Waves Node log level. Available values: `OFF`, `ERROR`, `WARN`, `INFO`, `DEBUG`, `TRACE`. More details about logging are available [here](https://docs.wavesplatform.com/en/waves-Node/logging-configuration.html). Can be overridden in a runtime using environment variable with the same name. |
|`WAVES_HEAP_SIZE`     | `2g`              | Default Waves Node JVM Heap Size limit in -X Command-line Options notation (`-Xms=[your value]`). More details [here](https://docs.oracle.com/cd/E13150_01/jrockit_jvm/jrockit/jrdocs/refman/optionX.html). Can be overridden in a runtime using environment variable with the same name. |

**Note: All build arguments are optional.**  

## Running Docker image

### Configuration options

1. The image supports Waves Node config customization. To change a config field use corrresponding JVM options. JVM options can be sent to JVM using `JAVA_OPTS` environment variable. Please refer to ([complete configuration file](https://raw.githubusercontent.com/wavesplatform/Waves/2634f71899e3100808c44c5ed70b8efdbb600b05/Node/src/main/resources/application.conf)) to get the full path of the configuration item you want to change.

```
docker run -v /docker/waves/waves-data:/var/lib/waves -v /docker/waves/waves-config:/etc/waves -p 6869:6869 -p 6862:6862 -e JAVA_OPTS="-Dwaves.rest-api.enable=yes -Dwaves.rest-api.bind-address=0.0.0.0 -Dwaves.wallet.password=myWalletSuperPassword" -e WAVES_NETWORK=stagenet -ti wavesplatform/wavesnode
```

2. Waves Node is looking for a config in the directory `/etc/waves/waves.conf` which can be mounted using Docker volumes. If this directory does not exist, a default configuration will be copied to this directory. Default configuration is chosen depending on `WAVES_NETWORK` environment variable. If the value of `WAVES_NETWORK` is not `mainnet`, `testnet` or `stagenet`, default configuration won't be applied. This is a scenario of using `CUSTOM` network - correct configuration must be provided. If you use `CUSTOM` network and `/etc/waves/waves.conf` is NOT found Waves Node container will exit.

3. By default, `/etc/waves/waves.conf` config includes `/etc/waves/local.conf`. Custom `/etc/waves/local.conf` can be used to override default config entries. Custom `/etc/waves/waves.conf` can be used to override or the whole configuration. For additional information about Docker volumes mapping please refer to `Managing data` item.

### Environment Variables

**You can run container with predefined environment variables:**

| Env variable                      | Description  |
|-----------------------------------|--------------|
| `WAVES_WALLET_SEED`        		| Base58 encoded seed. Overrides `-Dwaves.wallet.seed` JVM config option. |
| `WAVES_WALLET_PASSWORD`           | Password for the wallet file. Overrides `-Dwaves.wallet.password` JVM config option. |
| `WAVES_LOG_LEVEL`                 | Node logging level. Available values: `OFF`, `ERROR`, `WARN`, `INFO`, `DEBUG`, `TRACE`. More details about logging are available [here](https://docs.wavesplatform.com/en/waves-Node/logging-configuration.html).|
| `WAVES_HEAP_SIZE`                 | Default Java Heap Size limit in -X Command-line Options notation (`-Xms=[your value]`). More details [here](https://docs.oracle.com/cd/E13150_01/jrockit_jvm/jrockit/jrdocs/refman/optionX.html). |
|`WAVES_NETWORK`                    | Waves Blockchain network. Available values are `mainnet`, `testnet`, `stagenet`.|
|`JAVA_OPTS`                        | Additional Waves Node JVM configuration options. 	|

**Note: All variables are optional.**  

**Note: Environment variables override values in the configuration file.** 


### Managing data
We recommend to store the blockchain state as well as Waves configuration on the host side. As such, consider using Docker volumes mapping to map host directories inside the container:

**Example:**

1. Create a directory to store Waves data:

```
mkdir -p /docker/waves
mkdir /docker/waves/waves-data
mkdir /docker/waves/waves-config
```

Once container is launched it will create:

- three subdirectories in `/docker/waves/waves-data`:
```
/docker/waves/waves-data/log    - Waves Node logs
/docker/waves/waves-data/data   - Waves Blockchain state
/docker/waves/waves-data/wallet - Waves Wallet data
```
- `/docker/waves/waves-config/waves.conf` - default Waves config


3. If you already have Waves Node configuration/data - place it in the corresponsing directories


4. *Configure access permissions*. We use `waves` user with predefined uid/gid `143/143` to launch the container. As such, either change permissions of the created directories or change their owner:

```
sudo chmod -R 777 /docker/waves
```
or
```
sudo chown -R 143:143 /docker/waves      <-- prefered
```

5. Add the appropriate arguments to ```docker run``` command: 
```
docker run -v /docker/waves/waves-data:/var/lib/waves -v /docker/waves/waves-config:/etc/waves -e WAVES_NETWORK=stagenet -e WAVES_WALLET_PASSWORD=myWalletSuperPassword -ti wavesplatform/wavesnode
```

### Blockchain state

If you are a Waves Blockchain newbie and launching Waves Node for the first time be aware that after launch it will start downloading the whole blockchain state from the other nodes. During this download it will be verifying all blocks one after another. This procesure can take some time.

You can speed this process up by downloading a compressed blockchain state from our official resources, extract it and mount inside the container (as discussed in the previous section). In this scenario Waves Node skips block verifying. This is a reason why it takes less time. This is also a reason why you must download blockchain state *only from our official resources*.

**Note**: We do not guarantee the state consistency if it's downloaded from third-parties.

|Network     |Link          |
|------------|--------------|
|`mainnet`   | http://blockchain.wavesplatform.com/blockchain_last.tar |
|`testnet`   | http://blockchain-testnet.wavesplatform.com/blockchain_last.tar  |
|`stagenet`  | http://blockchain-stagenet.wavesplatform.com/blockchain_last.tar |


**Example:**
```
mkdir -p /docker/waves/waves-data

wget -qO- http://blockchain-stagenet.wavesplatform.com/blockchain_last.tar --show-progress | tar -xvf - -C /docker/waves/waves-data

chown -R 143:143 /docker/waves/waves-data

docker run -v /docker/waves/waves-data:/var/lib/waves wavesplatform/Node -e WAVES_NETWORK=stagenet -e WAVES_WALLET_PASSWORD=myWalletSuperPassword -ti wavesplatform/wavesnode
```

### Network Ports

1. REST-API interaction with Node. Details are available [here](https://docs.wavesplatform.com/en/waves-Node/Node-configuration.html#section-530adfd0788eec3f856da976e4ce7ce7).

2. Waves Node communication port for incoming connections. Details are available [here](https://docs.wavesplatform.com/en/waves-Node/Node-configuration.html#section-fd33d7a83e3b2854f614fd9d5ae733ba).


**Example:**
Below command will launch a container:
- with REST-API port enabled and configured on the socket `0.0.0.0:6870`
- Waves node communication port enabled and configured on the socket `0.0.0.0:6868`
- Ports `6868` and `6870` mapped from the host to the container

```
docker run -v /docker/waves/waves-data:/var/lib/waves -v /docker/waves/waves-config:/etc/waves -p 6870:6870 -p 6868:6868 -e JAVA_OPTS="-Dwaves.network.declared-address=0.0.0.0:6868 -Dwaves.rest-api.port=6870 -Dwaves.rest-api.bind-address=0.0.0.0 -Dwaves.rest-api.enable=yes" -e WAVES_WALLET_PASSWORD=myWalletSuperPassword -ti  wavesplatform/wavesnode
```

Check that REST API is up by navigating to the following URL from the host side:
http://localhost:6870/api-docs/index.html