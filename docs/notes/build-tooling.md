---
purpose: Implementation notes for the sbt build (Global settings, test concurrency limits) and the docker image build
---

# Build tooling

## A second assignment of a Global key replaces the first, it does not add to it

`concurrentRestrictions` is Global-scoped, and `IntegrationTestsPlugin` used to set it (limiting `Tags.ForkedTestGroup` to `maxParallelSuites`) alongside `build.sbt`'s own `inScope(Global)` assignment (limiting `Tags.Test`). Two assignments of one key in one scope is last-writer-wins, and `.sbt` files load after an AutoPlugin's `projectSettings`, so `build.sbt` won and the plugin's limit never existed: `print Global/concurrentRestrictions` showed only `Limit test to 8`.

`-Dwaves.it.max-parallel-suites` was therefore a no-op; node-it's observed 4-way suite concurrency on a GitHub runner came from `Tags.limit(Tags.Test, min(SystemProcessors, 8))` with 4 processors, which coincidentally matched the flag CI passed. Both limits now live in one `Seq` in `build.sbt`; the plugin keeps only the `maxParallelSuites` setting itself.

`checkTestLimits` (`build.sbt`, sequenced into `compilePR`) asserts the ForkedTestGroup cap is actually in force, so a third assignment of the key fails the build instead of quietly winning. The `:=` deliberately replaces sbt's own default rule set rather than extending it; if you need one of those defaults back, add it to this `Seq` explicitly.

Two things to know when checking this by hand. `print Global/concurrentRestrictions` is the authoritative check; `inspect` will happily show a defining assignment that lost. And `sbt --client -D...` sets the property on the thin client, not on the long-lived server that evaluates the setting, so the flag appears to be ignored; verify a `-D`-driven setting with a plain `sbt -D... --batch` (what CI runs).

## Forked suites also carry Tags.Test, so that limit caps node-it too

Fixing the shadowing above still left `-Dwaves.it.max-parallel-suites` unable to go above 4 on a GitHub runner. sbt's `ForkTests` tags every forked group with `(ForkedTestGroup, 1)` *plus* the tags of `Test / testOnly` (or `Test / test`), which default to `(Tags.Test, 1)`, so each node-it suite counted against `Tags.limit(Tags.Test, min(SystemProcessors, 8))` as well, and the smaller of the two limits won. `IntegrationTestsPlugin` clears both `tags` settings for node-it, which leaves `ForkedTestGroup` as the only cap on suite concurrency. Other modules keep the `Tags.Test` limit. `checkTestLimits` also fails if `Tags.Test` reappears on node-it's test tasks. To check by hand: `show node-it/Test/testOnly/tags` must print an empty list.

## Docker image build: staged directories, not a tarball

`sbt stageForDocker` copies the node and grpc-server universal mappings straight into `docker/target`, the image build context, and `docker/Dockerfile` `COPY`s them in. It used to package both as `.tgz` (`buildTarballsForDocker`) and unpack them in a `RUN`, which cost a recompression plus an unpack per iteration:

- Third-party jars go to `target/lib`, Waves' own (`lib/com.wavesplatform.*`) jars and scripts to `target/app`, so the big layer is content-identical across builds that only changed node code and stays cached. The few third-party `com.wavesplatform` artifacts (`blst-java`, `curve25519-java`, `zwaves`) land in `app` too, which only costs them their layer caching.
- The image build downloads nothing. It used to `wget` the Linux Corretto and rocksdb jars by versions pinned in the Dockerfile, which could silently drift from `Dependencies.scala`. Now `dockerExcludedJars` keeps the cross-platform `rocksdbjni`/`conscrypt`/`AmazonCorrettoCryptoProvider` jars out of the context (the release tarball still carries them: it is the cross-platform artifact), and in their place the task stages the `linux64` rocksdb jar plus both Corretto Linux jars under `target/native/<arch>`, named the way docker names that platform in `TARGETARCH` so the Dockerfile can `COPY target/native/$TARGETARCH`.
- The `linux64` rocksdb jar is resolved through a `DockerNative` ivy configuration of `build.sbt`'s own. `% Optional` on the node project is not enough to keep an artifact out of the release artifacts: the universal package picks up Optional dependencies (that is how the Corretto jars reach it), so declaring it there would put a second rocksdb jar into the tarball and the deb.
- `stageForDocker` copies a file only when its size or mtime differs from the staged copy, and deletes staged files that are no longer wanted. The first keeps BuildKit's context transfer incremental (it detects an unchanged file by size and mtime too); the second keeps a removed or bumped dependency from lingering on the image classpath.
- `COPY --link` layers are built independently of the ones below them, but any later instruction that touches the filesystem forces BuildKit to materialize the merged result. Keep `WORKDIR` (the only such instruction left) *above* the `COPY`s; `VOLUME`/`HEALTHCHECK`/`STOPSIGNAL`/`ENTRYPOINT` are metadata-only. `--chown` must be numeric there: a `--link` layer has no user database.
- Replacing the `RUN tar` with `COPY` also takes the unpacking out of the emulated arm64 leg of the multi-platform build in `publish-docker-image.yml`; the image now has no `RUN` after user creation at all.
