# Ride runner

Allows running Ride without a local Waves Node:
* As a service that emulates Waves Node REST API: `/utils/script/evaluate`;
* As an application that allows to run Ride with a prepared state in a file.

## How to build

1. A common step from the repository root: `./build-with-docker.sh`
2. _Optional_. If you need a Docker image, run: `docker build -t wavesplatform/ride-runner ride-runner/docker`

### Artifacts

* DEB package: `ride-runner/target/waves-ride-runner_${version}_all.deb`
* Fat JAR for running RIDE with a prepared state: `ride-runner/target/waves-ride-runner-${version}.jar`
* Standalone app and service: `ride-runner/docker/ride-runner-targer/waves-ride-runner.tgz`. 
  It has the `waves-ride-runner_${version}` directory. Notable:
   * `/bin/ride-runner` - main entrypoint.
       * Runs `RideRunnerWithBlockchainUpdatesService` by default.
     * Use `-help` to see all available switches;
   * `/conf/application.ini` - JVM options.

## Service

### How to install and run

#### Docker

#### DEB package

1. Install the package: `dpkg -i ride-runner_${version}_all.deb`. It shouldn't start after installation.
2. _Optional_. Configure the service:
   1. Update the Java options in `/etc/ride-runner/application.ini`
   2. Update a custom configuration for service: `/etc/ride-runner/ride-runner.conf`
3. Start the service: `systemctl start ride-runner`
 
#### Standalone

1. Extract the archive: `tar -xzf waves-ride-runner.tgz`
2. _Optional_. Configure the service:
   1. Update the Java options in `waves-ride-runner-${version}/conf/application.ini`
   2. Write a custom configuration for service:
      1. Copy an example config to the `conf` directory: `cp waves-ride-runner-$version/doc/main.conf waves-ride-runner-$version/conf/`
      2. [See](./src/main/resources/ride-runner.conf) all available options.
3. Run the service:
   * Without a custom config: `./waves-ride-runner-${version}/bin/ride-runner`
   * With a custom config: `./waves-ride-runner-${version}/bin/ride-runner $(pwd)/waves-ride-runner-${version}/conf/main.conf`

### REST API

Is available on `6890` port by default. Available endpoints:
1. `/utils/script/evaluate` - works almost as on Waves Node HTTP API. See https://nodes.wavesnodes.com/api-docs/index.html#/utils/evaluateScript for more information about this endpoint.
2. `GET /ride/status` for health checks.

### Limitations

If you faced one of them, please issue a ticket on GitHub and tell us your use-case.

1. Asset scripts aren't supported as in `GET /utils/script/evaluate` of Waves Node REST API.
2. Unsupported RIDE functions. A script fails with an error if tries to run one of these functions:
   1. [isDataStorageUntouched](https://docs.waves.tech/en/ride/functions/built-in-functions/account-data-storage-functions#isdatastorageuntouched-address-alias-boolean)
   2. [transferTransactionById](https://docs.waves.tech/en/ride/functions/built-in-functions/blockchain-functions#transfertransactionbyid)

## How to run

Consider the following directory structure:

- `./config/local.conf` - a local configuration;
- `./data` - where `ride-runner` caches are stored.

### Configuration options

1. The image supports a config customization. To change a config field use corresponding JVM options. JVM options can be
   sent to JVM using `JAVA_OPTS` environment variable (`-e` argument for Docker). Please refer
   to ([complete configuration file](./src/main/resources/application.conf))
   to get the full path of the configuration item you want to change.
2. The service is looking for a config in the directory `/etc/ride-runner`. During image build, a default configuration
   will be
   copied to this directory. While running container if the value of `RIDE_NETWORK` is not `mainnet`, `testnet`
   or `stagenet`, a default configuration won't be enough for a correct service working.
3. By default, `/etc/ride-runne/rride-runner.conf` config includes `/etc/ride-runner/local.conf`.
   Custom `/etc/ride-runner/local.conf` can be used to override default config entries.
   Custom `/etc/ride-runner/ride-runner.conf` can be used to override or the whole configuration.

Example of `local.conf`:

```conf
kamon {
  # Enable metrics
  enable = true
  
  # Enable a Prometheus scraping endpoint on 9095
  modules.prometheus-reporter.enabled = true
}
```

### Environment variables

**You can run container with predefined environment variables:**

| Env variable     | Description                                                                                                                                                                                       |
|------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `RIDE_LOG_LEVEL` | Logging level. Available values: `OFF`, `ERROR`, `WARN`, `INFO`, `DEBUG`, `TRACE`                                                                                                                 |
| `RIDE_HEAP_SIZE` | Default Java Heap Size limit in -X Command-line Options notation (`-Xms=[your value]`). More details [here](https://docs.oracle.com/cd/E13150_01/jrockit_jvm/jrockit/jrdocs/refman/optionX.html). |
| `RIDE_NETWORK`   | Waves Blockchain network. Available values are `mainnet`, `testnet`, `stagenet`.                                                                                                                  |
| `JAVA_OPTS`      | Additional JVM configuration options.                                                                                                                                                             |

**Note: All variables are optional.**

**Note: Environment variables override values in the configuration file.**

### Image tags

You can use the following tags:

- `latest` - currrent version of Mainnet
- `vX.X.X` - specific version

### Docker run example

```sh
docker run \
  -v $(pwd)/config/local.conf:/etc/ride-runner/local.conf:ro \
  -v $(pwd)/data:/var/lib/ride-runner \
  -p 127.0.0.1:6869:6869 \
  -p 127.0.0.1:9095:9095 \
  -e RIDE_NETWORK="mainnet" \
  -e RIDE_HEAP_SIZE="1g" \
  -ti wavesplatform/ride-runner:latest
```

### Docker Compose example

```yaml
version: '3.8'
services:
  ride-runner:
    image: wavesplatform/ride-runner:latest
    restart: "unless-stopped"
    ports:
      - 127.0.0.1:6890:6890
      - 127.0.0.1:9095:9095
    environment:
      - RIDE_NETWORK=mainnet
      - RIDE_HEAP_SIZE=1g
    volumes:
      - ./ride-runner/config/local.conf:/etc/ride-runner/local.conf:ro
      - ./ride-runner/data:/var/lib/ride-runner
```

### Grafana

There is an [example](./doc/grafana-prometheus-dashboard.json) dashboard for Grafana and Prometheus.
