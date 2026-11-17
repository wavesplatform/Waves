# Waves Node

Reference implementation of the [Waves](https://waves.tech) blockchain node: transaction validation, block generation, peer-to-peer networking, the [REST API](https://docs.waves.tech/en/waves-node/node-api/), gRPC and the RIDE smart contract language. See the [node documentation](https://docs.waves.tech/en/waves-node/).

## Running a node

The recommended way is the container image published to [Docker Hub](https://hub.docker.com/r/wavesplatform/wavesnode) with every release:

```bash
docker run -d --name waves-node \
  -v waves-data:/var/lib/waves \
  -e WAVES_NETWORK=mainnet \
  -e WAVES_WALLET_PASSWORD=<password> \
  -p 6868:6868 -p 127.0.0.1:6869:6869 \
  wavesplatform/wavesnode:latest
```

- Tags: `latest` (latest stable release) and `X.Y.Z` (specific version).
- `WAVES_NETWORK`: `mainnet`, `testnet` or `stagenet`. The peer-to-peer port is `6868` on Mainnet, `6863` on Testnet, `6862` on Stagenet.
- `6869` is the REST API; keep it bound to localhost or behind a proxy.
- A custom config can be mounted at `/etc/waves/waves.conf`; individual settings can be overridden with `JAVA_OPTS="-Dwaves.<path>=<value>"`.

All image options (environment variables, volumes, extensions, private network image) are described in [docker/README.md](docker/README.md).

Alternatively, run the `waves-all-*.jar` from [releases](https://github.com/wavesplatform/Waves/releases) on JDK 17 or newer: `java -jar waves-all-X.Y.Z.jar waves.conf` (start from [node/waves-sample.conf](node/waves-sample.conf)). See [how to install a node](https://docs.waves.tech/en/waves-node/how-to-install-a-node/how-to-install-a-node).

A fresh node downloads and verifies the whole chain. To speed this up, see [synchronizing the blockchain](https://docs.waves.tech/en/waves-node/options-for-getting-actual-blockchain/).

## Configuration

- [Node configuration](https://docs.waves.tech/en/waves-node/node-configuration) and [logging](https://docs.waves.tech/en/waves-node/logging-configuration).
- Defaults: [application.conf](node/src/main/resources/application.conf), per-network overrides in [network-defaults.conf](node/src/main/resources/network-defaults.conf).
- [Extensions](https://docs.waves.tech/en/waves-node/extensions/), such as gRPC server and blockchain updates.

## Building

Requires JDK 17 and [sbt](https://www.scala-sbt.org/download/).

```bash
sbt node/assembly        # fat jar: target/out/jvm/u/node/waves-all-*.jar
sbt packageAll           # jars, deb packages and the docker build context
sbt stageForDocker && docker build -t waves-node docker   # local image
```

## Testing

```bash
sbt node-tests/testFull                  # node unit tests
sbt "node-tests/testOnly *SuiteName"     # single suite
sbt checkPR                              # what CI runs: format check, -Werror compile, all unit tests, packaging
```

Integration tests run nodes in Docker and need the `node-it` image built first:

```bash
sbt node-it/docker
sbt --server -Dwaves.it.max-parallel-suites=2 --batch "node-it/testOnly *SuiteName"
```

See [docs/notes/testing.md](docs/notes/testing.md) for details.

## Contributing

Open an issue to discuss major changes first. Pull requests target `version-1.6.x` and must pass `sbt checkPR`. Follow the [code of conduct](CODE_OF_CONDUCT.md).

## Links

- [Documentation](https://docs.waves.tech/)
- Explorer: [Mainnet](https://wavesexplorer.com/), [Testnet](https://testnet.wavesexplorer.com/), [Stagenet](https://stagenet.wavesexplorer.com/)
- [Ride IDE](https://waves-ide.com/)

## License

[MIT](LICENSE)

## Acknowledgements

[![JProfiler](https://www.ej-technologies.com/images/product_banners/jprofiler_small.png)](https://www.ej-technologies.com/jprofiler)

Performance work on the node uses [JProfiler, the Java profiler](https://www.ej-technologies.com/jprofiler) by ej-technologies, provided under their open source license.
