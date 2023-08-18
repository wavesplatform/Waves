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
    * `/bin/waves-ride-runner` - main entrypoint.
        * Runs `RideRunnerWithBlockchainUpdatesService` by default.
        * Use `-help` to see all available switches;
    * `/conf/application.ini` - JVM options.

## Service

A default layout:

* `/var/lib/waves-ride-runner` - the data.
* `/etc/waves-ride-runner` - configs.

### How to install and run

#### Docker

##### Docker run

```shell
docker run \
  -v $(pwd)/data:/var/lib/waves-ride-runner \
  -v $(pwd)/config/local.conf:/etc/waves-ride-runner/local.conf:ro \
  -p 127.0.0.1:6890:6890 \
  -p 127.0.0.1:9095:9095 \
  -e RIDE_HEAP_SIZE="1g" \
  -ti wavesplatform/ride-runner:latest
```

##### Docker Compose

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
      - RIDE_HEAP_SIZE=1g
    volumes:
      - ./waves-ride-runner/data:/var/lib/waves-ride-runner
      - ./waves-ride-runner/config/local.conf:/etc/waves-ride-runner/local.conf:ro
```

##### Configuration options

1. The image supports a config customization. To change a config field use corresponding JVM options. JVM options can be
   sent to JVM using `JAVA_OPTS` environment variable (`-e` argument for Docker). Please refer
   to ([complete configuration file](./src/main/resources/ride-runner.conf)) to get the full path of the configuration
   item you want to change.
2. The service is looking for a config in `/etc/waves-ride-runner/main.conf`. During image build, a default
   configuration will be copied to this directory.
    * A default configuration is enough for a correct service working on MainNet;
    * While running container if the value of `WAVES_NETWORK` is not `mainnet`, `testnet` or `stagenet`, default
      configuration won't be enough for correct working. This is a scenario of using `CUSTOM` network - correct
      configuration must be provided when running container.
3. By default, `/etc/waves-ride-runner/main.conf` config includes `/etc/waves-ride-runner/local.conf`.
    * Custom `local.conf` can be used to override default config entries.
    * Custom `main.conf` can be used to override or the whole configuration.

Example of `local.conf`:

```conf
kamon {
  # Enable metrics
  enable = true
  
  # Enable a Prometheus scraping endpoint on 9095
  modules.prometheus-reporter.enabled = true
}
```

##### Environment variables

**You can run container with predefined environment variables:**

| Env variable     | Default   | Description                                                                                                                                                                                       |
|------------------|:----------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `RIDE_LOG_LEVEL` | `INFO`    | Logging level. Available values: `OFF`, `ERROR`, `WARN`, `INFO`, `DEBUG`, `TRACE`                                                                                                                 |
| `RIDE_HEAP_SIZE` | `2500m`   | Default Java Heap Size limit in -X Command-line Options notation (`-Xms=[your value]`). More details [here](https://docs.oracle.com/cd/E13150_01/jrockit_jvm/jrockit/jrdocs/refman/optionX.html). |
| `RIDE_NETWORK`   | `mainnet` | Waves Blockchain network. Available values are `mainnet`, `testnet`, `stagenet`.                                                                                                                  |
| `JAVA_OPTS`      |           | Additional JVM configuration options. Some options are mandatory and specified in [entrypoint.sh](./docker/entrypoint.sh)                                                                         |

Notes:

1. All variables are optional.
2. Environment variables override values in the configuration file.

##### Image tags

You can use the following tags:

- `latest` - current version of Mainnet;
- `vX.X.X` - specific version.

#### DEB package

1. Install the package: `dpkg -i waves-ride-runner_${version}_all.deb`. It shouldn't start after installation.
2. _Optional_. Configure the service:
    1. Update the Java options in `/etc/waves-ride-runner/application.ini`
    2. Update a custom configuration for service: `/etc/waves-ride-runner/main.conf`
       See [ride-runner.conf](./src/main/resources/ride-runner.conf) for all available options.
3. Start the service: `systemctl start waves-ride-runner`

#### Standalone

1. Extract the archive: `tar -xzf waves-ride-runner.tgz`
2. _Optional_. Configure the service:
    1. Update the Java options in `waves-ride-runner-${version}/conf/application.ini`
    2. Write a custom configuration for service:
        1. Copy an example config to the `conf`
           directory: `cp waves-ride-runner-$version/doc/main.conf waves-ride-runner-$version/conf/`
        2. See [ride-runner.conf](./src/main/resources/ride-runner.conf) for all available options.
3. Run the service:
    * Without a custom config: `./waves-ride-runner-${version}/bin/waves-ride-runner`
    * With a custom
      config: `./waves-ride-runner-${version}/bin/waves-ride-runner ./waves-ride-runner-${version}/conf/main.conf`

### REST API

Is available on `6890` port by default. Available endpoints:

1. `/utils/script/evaluate` - works almost as on Waves Node HTTP API.
   See https://nodes.wavesnodes.com/api-docs/index.html#/utils/evaluateScript for more information about this endpoint.
2. `GET /ride/status` for health checks.

### Limitations

If you faced one of them, please issue a ticket on GitHub and tell us your use-case.

1. Asset scripts aren't supported as in `GET /utils/script/evaluate` of Waves Node REST API.
2. Unsupported RIDE functions. A script fails with an error if tries to run one of these functions:
    1. [isDataStorageUntouched](https://docs.waves.tech/en/ride/functions/built-in-functions/account-data-storage-functions#isdatastorageuntouched-address-alias-boolean)
    2. [transferTransactionById](https://docs.waves.tech/en/ride/functions/built-in-functions/blockchain-functions#transfertransactionbyid)

## Running Ride with prepared state

You need an input file, that contains a description of the blockchain state
in [HOCON](https://github.com/lightbend/config/blob/main/HOCON.md).

See:
* The documented [example](./src/test/resources/sample-input.conf).
* Handy [variables](./src/main/resources/cli-default-options.conf) those included by default.

How to run:

```shell
java -jar waves-ride-runner-${version}.jar ./sample-input.conf
```

You should see the result in JSON.

Help:

```shell
java -jar waves-ride-runner-1.4.18-8f8a0f98d3a2304d9b05c369bd333c8f85044e75-DIRTY.jar --help
```

**Important note**: using multiple different networks may cause errors if you run multiple scripts (for example, if you
passed files with glob pattern).

### Functionality

* Running one or multiple scripts;
* Different run modes:
  * A regular run to see what script returns;
  * Running tests;
* Human-readable and JSON outputs;
* Creating a JUnit report.

Run with `--help` to see more details.

## Grafana

There is an [example](./doc/grafana-prometheus-dashboard.json) dashboard for Grafana and Prometheus.
