# @waves/ride-js

JavaScript/TypeScript compiler and REPL for the [Ride](https://docs.waves.tech/en/ride/)
smart-contract language. Self-contained: the RIDE compiler and REPL (ScalaJS) are bundled
**once**, with no runtime dependency on `@waves/ride-lang` or `@waves/ride-repl`.

## Install

```
npm install @waves/ride-js
```

## Usage

```js
const RideJS = require("@waves/ride-js");

const { result, error } = RideJS.compile("true");
// result: { bytes, base64, size, ast, complexity, verifierComplexity, callableComplexities, ... }

const limits = RideJS.contractLimits;
const repl = RideJS.repl();
```

The same `RideJS` global is exposed by the browser bundle (`dist/ride.min.js`).

## Layout

- `src/`: TypeScript glue (`index.ts` public API, `interop.ts` crypto/http globals) plus the
  hand-maintained `index.d.ts` public typings, which `package.json` references directly (no copy);
  `test/api.test.ts` fails when they drift from the compile result.
- `scalajs/ride-scalajs.js`: the single combined ScalaJS artifact (the linked `repl-js`
  output, which exports both the compiler API and the repl API). The Waves `sbt replJS/fullOptJS`
  task is configured to emit it **directly** here (see `repl/js/build.sbt`; `fastOptJS` stays under `target/`), so there is no copy
  step.
- `dist/`: build output: `index.js` (node `main`), `interop.js`, `ride.min.js` (browser).
- `pnpm-workspace.yaml`: pnpm settings: release-age policy and exclusions, plus `allowBuilds` install-script decisions when a dependency needs one (none today).

## Build & test

This package uses **pnpm** (pinned via the `packageManager` field). pnpm does not run dependency
install scripts: a dependency that ships one fails the install until it is explicitly allowed or
denied under `allowBuilds` in `pnpm-workspace.yaml`. No current dependency has one, so no
third-party code executes on install. pnpm also refuses versions published less than a day ago
(`minimumReleaseAge`); to take a fresh release (e.g. a security fix), list its exact
`name@version` under `minimumReleaseAgeExclude`, and drop the entry once it is a day old.

```
sbt replJS/fullOptJS        # from the Waves repo root: emits ride-js-bundle/scalajs/ride-scalajs.js
cd ride-js-bundle
pnpm install --frozen-lockfile
pnpm run build              # build:ts -> build:browser (consumes the emitted artifact)
pnpm test                   # build, tsc over test/, then Vitest: the public API, the packed tarball and ride.min.js
pnpm run format             # Prettier; CI runs format:check
```

Test fixtures are picked at random from a seed printed as `RIDE_TEST_SEED=<n>` at the start of a run; rerun with that variable set to reproduce a failure.

## Releasing

The version in `package.json` is independent of the node version. A Waves GitHub release runs `.github/workflows/publish-to-npmjs.yml`, which publishes that version with the `latest` tag, or skips publishing when it is already on npm; bump it in the PR that changes the package. A manual run of the workflow with an empty version publishes the next `<version>-snapshot.N` with the given tag (default `snapshot`); with a version, it publishes exactly that version and tag. Snapshots sort before their release, so bump the version after releasing it.

### Dependency fixes between node releases

Whether a vulnerable dependency needs a release depends on how it reaches consumers:

- `devDependencies` are never shipped: merge the bump, no release.
- Runtime `dependencies` are shipped as semver ranges (our lockfile is not): consumers pick up a fix within the range on their next install. Release only when the fix is outside the range; raise the range floor in that release.
- Code inlined into `dist/ride.min.js` (runtime dependencies, Node polyfills) or `scalajs/ride-scalajs.js` (Scala dependencies) is frozen at build time: release.

To release:

1. In the fix PR, bump the patch version in `package.json`.
2. Check `git diff <last node release tag> -- lang/ repl/`: a release from `version-1.6.x` ships that compiler. If it has unreleased RIDE changes, ask a release approver how to publish from the release tag instead.
3. After merging, run `Publish packages to NPMJS` on `version-1.6.x` with that version and tag `latest`; a release approver approves the `NPMJS` environment.
4. The next node release finds the version on npm and skips it.
