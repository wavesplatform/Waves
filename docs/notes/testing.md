---
purpose: Implementation notes for testing (node-it ports, load tests, node-tests debugging)
---

# Testing: node-it and node-tests

## node-it: published ports

`Docker.startNodeInternal` publishes only the container ports a suite actually reaches from the host, one explicit `PortBinding.randomPort` each, never `publishAllPorts`.

Publishing everything the image `EXPOSE`s was a real CI failure, not just waste: `publishAllPorts` binds each of the image's ports on both `0.0.0.0` and `::`, taking host ports out of `ip_local_port_range`, and dockerd shares that range with every outbound connection the test JVM makes. Under CI's parallelism container starts began failing with `driver failed programming external connectivity ... failed to listen on TCP socket: address already in use`, which aborts the whole suite because `startNodeInternal` has no retry.

Which ports a node needs is derived from its own resolved config: the REST port always, `waves.grpc.port` iff `waves.extensions` contains `GRPCServerExtension`, `waves.blockchain-updates.grpc-port` iff it contains `BlockchainUpdates`. Read that list from `actualConfig`, never from the suite's own `nodeConfig` fragment, which does not see `template.conf` or `suiteConfig`. The gRPC extension is therefore off in `template.conf` (`extensions = []`) and switched on per suite by `GrpcIntegrationSuiteWithThreeAddress`'s `createDocker`. That override belongs on `GrpcIntegrationSuiteWithThreeAddress`, not on a narrower trait: a suite that mixes in the former directly would otherwise get no gRPC port.

An exposed-but-unpublished port is a key in docker's port map with an *empty* binding list, not a missing entry, so `externalPort`'s guard rejects emptiness as well as `null`; a `null` check alone lets it through to an `IndexOutOfBoundsException: Index: 0` that says nothing about which port was missing.

Two ports do not follow from the config. The node-to-node port is always listening inside the container, but nodes peer over the container network (`containerNetworkAddress`, and `declared-address` is the container IP), so it only needs publishing for a suite that speaks the binary protocol from the test JVM itself: `Docker`'s `publishNetworkPort` flag (e.g. `SimpleTransactionsSuite`). The debugger port is published only under `enableDebugger`.

Consequences: `NodeInfo.hostNetworkAddress` must stay `lazy`, or constructing any node without a published network port dies; `externalPort` fails with the port number and the published set rather than an NPE; and a suite that starts using a new port has to say so, by enabling the extension that owns it or by adding a flag. Bindings use an empty host ip so they stay dual-stack, since suites connect to `localhost`, which resolves to `::1` first on some hosts.

## Load tests

The `@LoadTest`-annotated suites (`sbt "print node-it/Test/loadTestNames"` for the current list) run in their own CI job (`run-load-tests` in `check-pr.yaml`), through `node-it/loadTests` and pinned to `-Dwaves.it.max-parallel-suites=1`; the `run-integration-tests` job runs `node-it/integrationTests`, the complement. Do not fold them back into the main job. They are two explicit jobs rather than a matrix so the required-check names stay stable, which means action pins have to be bumped in both.

These suites measure whether a node keeps up with thousands of transactions, so anything else on the same machine turns them into a measurement of the runner. Sharing a 4 vCPU runner with other node-it suites made `WideStateGenerationSuite` fail consistently: broadcasts slowed to seconds each, and the miner's UTX pool sat near empty while its peers held thousands, so the "UTX is empty" wait never converged.

That last shape looks like a propagation bug and is not one: `TransactionSynchronizer` feeds network transactions through `.whileBusyBuffer(OverflowStrategy.DropNew(maxQueueSize))`. A node whose validation pipeline falls behind **silently drops** incoming transactions, logs nothing, and has no way to re-request them; the sender only re-offers on its own rebroadcast cycle. A CPU-starved miner therefore starves itself of work while its peers hold the pool. Expect this (miner UTX near zero, peers' pools stuck or growing) whenever a node-it suite is resource-starved rather than functionally broken.

Both halves are selected by `loadTestNames` (`project/IntegrationTestsPlugin.scala`), which partitions `definedTests` by `isAnnotationPresent` through the test classloader: tagging a suite `@LoadTest` moves it in both directions at once, and there is no tag string in the workflow. The annotation is `@Target(ElementType.TYPE)` on purpose; discovery is class-level, so a method-level tag would be excluded from the integration job and never selected by the load job, i.e. silently stop running.

Selecting by name rather than by ScalaTest `-n`/`-l` tag arguments is deliberate. A renamed or mistyped tag in `-n` selects nothing, and sbt reports `No tests were executed` and **exits 0**: green CI running no load tests. The same drift on `-l` silently puts the load suites back in the main job. Deriving both halves from one `LoadTestTag` constant makes a rename a `ClassNotFoundException` and an empty selection a `sys.error`. (`-n` is also bypassed for a suite named on `testOnly`: ScalaTest rebuilds the filter around `Suite.SELECTED_TAG` for an explicitly specified suite, so `testOnly X -- -n Some.Tag` runs X in full.)

`loadTests` and `integrationTests` both go through `testOnly`, both refuse to run an empty selection, and `loadTests` additionally refuses to start when `SCALATEST_EXCLUDE_TAGS` would filter out the very tag it selects on.

## node-tests: finding why a block was rejected

Tests log at `OFF` by default (`node/tests/src/test/resources/logback-test.xml`), so an append rejected inside the appender surfaces only as `Can't apply block ..., see logs` from `Domain.appendBlock`. Re-run with `-Dlogback.test.level=TRACE` and grep for the real reason.
