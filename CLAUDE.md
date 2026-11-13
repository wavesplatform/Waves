---
purpose: Canonical agent instructions for the Waves node repo (AGENTS.md is a thin pointer)
---

# Waves node: agent instructions

Waves node in Scala 3 (consensus, state, REST/gRPC API, RIDE). sbt 2.x multi-module build; CI runs JDK 17 (temurin). README.md covers node quickstart; keep this file short and laconic.

## Build / test

- `sbt compilePR`: clean, `scalafmtCheck`, compile everything (Test included) with `-Werror`, then `checkTestLimits`; warnings block.
- `sbt checkPR`: `compilePR` + unit tests (`lang-tests`, `lang-tests-js`, `repl-jvm`, `node-tests`, `grpc-server`, `ride-runner`) + `fullOptJS` for `lang-js`/`repl-js` + `node/assembly` + `stageForDocker`. This is what CI (`check-pr.yaml`) runs.
- `sbt node-tests/testFull`: unit tests; single suite via `sbt "node-tests/testOnly *SuiteName"`. sbt 2's `test` is incremental: it reruns only what changed and prints `[success]` having run nothing, so it never proves a module green; `testFull` is the real run and what `checkPR` uses. Chain several tasks with one quoted `;` argument (`sbt "a/testFull; b/testFull"`).
- sbt 2's `sbt` is a thin client that forks a background server and reuses it across runs. CI runs `sbt --server ... --batch` (foreground server, no client): see "sbt 2" in `docs/notes/build-tooling.md` before changing that or any task with side effects outside `target/`.
- Build outputs live under `target/out/<platform>/u/<project>/` at the repo root (e.g. `target/out/jvm/u/node/waves-all-*.jar`), not under each module's `target/`.
- `sbt "node-it/docker;node-it/testOnly ..."`: integration tests (needs Docker); slow, don't run by default. `node-it/docker` builds `com.wavesplatform/node-it:latest` from whatever `node`/`grpc-server` currently compile to; it is not rebuilt automatically, so re-run it by hand after touching either module's sources.
- `sbt node-it/loadTests` / `sbt node-it/integrationTests`: the `@LoadTest`-annotated suites and their complement, discovered from the annotation. They run on separate runners (entries of the `node-it` matrix in `check-pr.yaml`); the load half runs serially, the other half is split across two runners by `-Dwaves.it.shard=i/n`. Read "Load tests" in `docs/notes/testing.md` before touching one, or before changing how node-it is invoked in CI.
- Sandboxed dev environments: the default Docker builder here cannot do nested overlayfs mounts, so `node-it/docker` (or any `docker build` using the default `docker` driver) fails every `RUN` layer with `mount source: "overlay", ... err: operation not permitted`. Switch to a docker-container buildx builder (`docker buildx ls`; `docker buildx create --use` if none exists) and build by hand: `cd docker && docker buildx build --load -t com.wavesplatform/node-it:latest .`. `sbt node-it/docker` always uses the default builder, but its dependency `stageForDocker` still stages the node files into `docker/target`, so letting it fail once before the manual build is a reasonable sequence.
- node-it suites run with `-Dwaves.it.max-parallel-suites=N` to cap Docker resource usage (each suite starts its own containers). Pass it as `sbt --server -D... --batch`, not inside the sbt shell and not through the thin client (`sbt -D...` or `sbt --client`), which keeps the property to itself; see `docs/notes/build-tooling.md`. Per-suite logs land in `target/out/jvm/u/node-it/logs/<run-id>/<abbreviated.suite.Name>/`, but the real assertion/exception message is only in the sbt console output; grep it for `*** FAILED ***`/`*** ABORTED ***` and the lines after.
- `sbt scalafmt`: auto-format before committing. CI checks only `Compile` sources; `scalafmtAll` also rewrites unformatted test files nobody touched.
- The long-lived sbt server (any thin-client `sbt` run) that loads the Amazon Corretto crypto provider after a recompile can abort with `IllegalAccessError: ... ZombieClassLoader` (the provider registers once per JVM, from the old layer). `sbt --client shutdown`, then rerun.

## Modules

`lang` (jvm/js, plus `lang/testkit`, `lang/tests`, `lang/tests-js`), `node` (core), `node/testkit` (shared test base classes + generators), `node/tests` (unit tests), `grpc-server`, `node-it` (Docker integration tests), `node-generator`, `benchmark`, `ride-runner`, `repl` (cross JVM/JS), `curve25519-test`.

## Conventions

- In-repo skills live in `.claude/skills/`: `scala-conventions`, `engineering-philosophy`, `running-tdd-cycles`, `committing-changes`, `reviewing-changes`. They are the source of truth for code style, TDD, and review.
- In-repo review subagents live in `.claude/agents/` (code-reviewer, security-auditor, architect-review, acceptance-auditor). Each runs one pass of `reviewing-changes` in a clean context; `/review` launches all four in parallel and aggregates.
- Comments are short and explain why, not what.
- Every `.md` file under `docs/notes/` and `.claude/` carries YAML frontmatter with `purpose:` (SKILL.md files use skill frontmatter, `name:` + `description:`, instead); paragraphs are single unwrapped lines.
- No em-dash (U+2014) in any file.
- GitHub Actions pinned by commit SHA (`@<sha> # vX.Y.Z`).

## PR workflow

- Default branch is `version-1.6.x`. Never push to it; feature branch + PR, human merges.
- Before pushing: `sbt compilePR` locally; fix formatting with `sbt scalafmt`, never by hand-matching the checker. Then run `/review` and fix Critical/Major findings in the same branch.
- After pushing: watch `gh pr checks` until green; a red check is yours to fix or explicitly hand over.
- On a mistake repeated twice, encode the rule (lint config, test, CI, `docs/notes/` for implementation learnings, or this file for build/conventions/workflow rules only); never just fix the instance.

## Implementation notes

Hard-won implementation knowledge lives in `docs/notes/`, one file per subsystem; the matching file MUST be read before working in its area. New learnings go into the matching topic file plus one trigger line here, never as new always-loaded text in CLAUDE.md.

- Writing or fixing tests (node-it suites and fixtures, Docker.scala ports, load tests, node-tests helpers): read `docs/notes/testing.md` first.
- Touching the BlockchainUpdates extension or events.StateUpdate: read `docs/notes/blockchain-updates.md` first.
- Touching block or microblock mining (Miner.scala, MicroBlockMinerImpl), or a test that reacts to the liquid block: read `docs/notes/mining.md` first.
- Touching BlockchainUpdaterImpl (processBlock, processMicroBlock, removeAfter, the read/write lock): read `docs/notes/blockchain-updater.md` first.
- Touching build.sbt, sbt plugins or tasks (IntegrationTestsPlugin, concurrentRestrictions, checkTestLimits), or the docker image (Dockerfile, entrypoint.sh, stageForDocker): read `docs/notes/build-tooling.md` first.
