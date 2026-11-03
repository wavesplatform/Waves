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

## node-it: is the runner saturated?

Both node-it CI jobs sample the runner into the uploaded logs while sbt runs: `cpu.log` (the first line of `/proc/pressure/cpu`, plus the 1-minute load average, every 5s) and `vmstat.log` (`vmstat -t 5`), both in `node-it/target/logs/<run-id>/`. Read them against a timeline of the suites, built from the first and last timestamps of each suite's `test.log`. Do not use the JUnit XML `timestamp`/`time`, which leave out container startup and are misleading. Rough reading: CPU pressure `some avg10` well under 20% with `vmstat` `id` above 0 means there is room for more parallel suites; a sustained `avg10` above about 40%, or a run queue (`r`) above the vCPU count, means the runner is saturated, which leads to the starvation described under Load tests. Also check `wa` and `free`: every node container gets `-Xmx2g`, so memory can run out before CPU does.

## node-it: wait for the transaction, not for the next block

Most node-it suites are slow because of how they wait, not because of how much they check. Before the merge, `IssueReissueBurnAssetSuite` and its gRPC twin each spent about 4.4 of their 7.5 minutes in `height >= N` waits (about 105 per suite). Three helpers hide such a wait: `waitForHeightAriseAndTxPresent`, `nodes.waitForHeightArise()` after a broadcast, and, less obviously, every `SyncGrpcApi` broadcast with `waitForTx = true`, which goes through `waitForTxAndHeightArise`. The REST `waitForTx = true` waits only for the transaction. With `micro-block-interval = 1s` a transaction lands in the liquid block within about a second, and both REST and gRPC serve liquid state (balances, asset info, state changes, transactions by address), so on a single node a plain transaction wait is enough. Keep a block wait only where the API reads solid state (`/assets/{id}/distribution`), where the test is about heights (rollback, activation, depth), or where another node has to have the block. The default 1s poll in `waitForTransaction` adds up too: across ~165 waits, polling every 200ms took the merged suite from 4:00 to 3:15. The liquid block is not final, though: the miner can orphan its own newest microblock ("Key block forged on a stale parent" in `docs/notes/mining.md`), taking its transactions out of the state for about 200ms. A transaction that a later broadcast is validated against (funding, set-script, issue before reissue or burn) has to be waited for until the next key block is on top of it and it is still at the same height; `settled` in `IssueReissueBurnAssetSuite` does that. Reads right after a plain transaction wait share the same window and can still flake, more rarely.

## node-tests: finding why a block was rejected

Tests log at `OFF` by default (`node/tests/src/test/resources/logback-test.xml`), so an append rejected inside the appender surfaces only as `Can't apply block ..., see logs` from `Domain.appendBlock`. Re-run with `-Dlogback.test.level=TRACE` and grep for the real reason.
