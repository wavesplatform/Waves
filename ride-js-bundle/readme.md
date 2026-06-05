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
const RideJS = require('@waves/ride-js');

const { result, error } = RideJS.compile('true');
// result: { bytes, base64, size, ast, complexity, verifierComplexity, callableComplexities, ... }

const limits = RideJS.contractLimits;
const repl = RideJS.repl();
```

The same `RideJS` global is exposed by the browser bundle (`dist/ride.min.js`).

## Layout

- `src/` — TypeScript glue (`index.ts` public API, `interop.ts` crypto/http globals) plus the
  hand-maintained `index.d.ts` public typings, which `package.json` references directly (no copy).
- `scalajs/ride-scalajs.js` — the single combined ScalaJS artifact (the linked `repl-js`
  output, which exports both the compiler API and the repl API). The Waves `sbt replJS/fullOptJS`
  task is configured to emit it **directly** here (see `repl/js/build.sbt`), so there is no copy
  step.
- `dist/` — build output: `index.js` (node `main`), `interop.js`, `ride.min.js` (browser).
- `pnpm-workspace.yaml` — pnpm settings: the `allowBuilds` install-script policy + dependency
  `overrides`.

## Build & test

This package uses **pnpm** (pinned via the `packageManager` field). pnpm does not run dependency
install scripts: the policy lives in `pnpm-workspace.yaml` under `allowBuilds`, where every
dependency that ships an install script must be explicitly allowed or denied. Ours are denied
(`esbuild: false`), so no third-party code executes on install; a *new* dependency with a build
script fails the install until a deliberate decision is recorded.

```
sbt replJS/fullOptJS        # from the Waves repo root: emits ride-js-bundle/scalajs/ride-scalajs.js
cd ride-js-bundle
corepack enable             # makes the pinned pnpm available
pnpm install --frozen-lockfile
pnpm run build              # build:ts -> build:browser (consumes the emitted artifact)
pnpm test                   # Vitest suite (compiles RIDE through the public API)
```
