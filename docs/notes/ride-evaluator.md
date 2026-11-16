---
purpose: Implementation notes for the RIDE evaluator (EvaluatorV2, EvaluationResult, EvaluationCopy) and how to measure its speed
---

# RIDE evaluator

## The evaluator rewrites the script tree in place

`EvaluatorV2` evaluates by writing results back into the tree: `FUNCTION_CALL.args`, `BLOCK.body`, `GETTER.expr`, `IF.cond` and `LET.value` are `var`s, and a reference to a `LET` replaces its value with the evaluated one, which is how LETs are lazy and evaluated once. So it must never run on the tree a script holds, which is shared between invocations and threads. `EvaluationCopy` makes the copy it runs on, and copies only what gets written into before the evaluator reaches it:

- Leaves (`REF`, evaluated constants, `FAILED_*`) are immutable and shared.
- Function declarations are shared: a user function call copies the function body (`evaluateUserFunction`) and evaluates the copy, never the body in place.
- A `LET` is a new node whose value is still the original's, marked with `LET.valueShared`. `evaluateRef` copies the value when it first evaluates it, so values nothing references are never copied. `valueShared` is a body field, not part of `LET`'s equality.

Anything that starts evaluating a tree, or adds a path that writes into one, has to keep to this: a node written into must have been copied first. In particular, never rebuild a `LET` (`copy`, `LET(name, value)`) between `EvaluationCopy` and its first evaluation: the new node drops `valueShared` while still holding the original value, and the evaluator would write into the shared tree. `EvaluatorV2Test` evaluates one tree twice and checks it is unchanged afterwards.

`EXPR.deepCopy` (through `cats.Eval`) is no longer used by the evaluator and is kept on purpose: it is the reference `EvaluationCopyTest` compares against and the baseline in `FunctionCallCopyBenchmark`. It allocated several objects per node and copied whole dApps for every invocation.

## EvaluationResult is a trampoline, not Monix

`EvaluationResult` used to wrap `Coeval[Either[...]]`, so every evaluator step went through Monix's run loop and allocated a `Coeval` and an `Either`. It is now its own small trampoline: immutable step descriptions (`flatMap`, `map`, `transform`, `handleErrorWith`, `delay`, `defer`) run by `run()` with a stack local to the run. It keeps Coeval's behaviour where results depend on it: an error value skips the following `flatMap`/`map` steps, a thrown non-fatal exception unwinds to the nearest `handleErrorWith` or out of `run()`, and `applyLimitedCoeval` still returns a `Coeval` that turns an exception into `Left((e.getMessage, limit, log))`. Error texts of failed transactions end up in the state, so these paths must not change. `Extended` native functions (`invoke` and friends) still return a `Coeval`, run with `fromCoeval`.

An invoke between dApps evaluates the callee in a nested run on the same thread (the `Extended` function's `Coeval`, then a new `run()`), so the call stack grows with the depth of the chain, up to 100 invokes. A `StackOverflowError` is fatal and nothing catches it, so nodes with different stack sizes could disagree about a block. `SyncDAppLimits` runs the deepest allowed chain on a thread with a 1 MB stack to keep the margin visible.

Monix stack tracing (`monix.eval.stackTracingMode`, `cached` by default) cost 7% of the slice below while every evaluator step ran on `Coeval`. Since the trampoline it makes no difference (2146124 to 2176100 on one machine: 269.6 s with it off, 269.5 s with it on), so the node keeps the Monix default.

## Script estimation

A `SetScript` transaction estimates every callable of the dApp against the whole dApp, once for the current estimator and once for every later one (`SetScriptTransactionDiff.estimate`), then the verifier (`DiffsCommon.countVerifierComplexity`). The estimators run on `TaskMT`, a state monad over `cats.Eval`, so this costs like the evaluator did before the changes above. Past testnet height 2.1M scripts are set over and over: from 2146124 to 2408350, 10,763 `SetScript` transactions set 3,587 distinct scripts, the most common one 270 times. `SetScriptTransactionDiff` keeps the complexities of the last 1,000, keyed by a hash of the script bytes and `EstimationSettings`, every blockchain setting the estimation reads (estimator, RideV6, reduced verifier limit, estimation overflow check, V1 pre-check). The estimation takes `EstimationSettings` instead of a `Blockchain`, so a new setting it needs has to go into that class and with it into the key; reading the blockchain directly again would let a script set under the new setting get the old complexities. `SetScriptEstimationCacheTest` sets one script under settings that estimate differently. Moving the estimators off `cats.Eval` would be a larger, consensus-relevant change, since stored complexities depend on them.

## Measuring

Import a dApp-heavy stretch of testnet: import once to 2146124 (`waves import -i <blocks-file> -h 2146124`), keep a copy of that `data` directory, and time each variant importing from a fresh copy of it with `-h 2176100`. Over those 30k blocks 94% of block appending is `InvokeScriptTransactionDiff`. With the JProfiler agent started from a config file, take intermediate snapshots with `jpcontroller -n <pid>` (`saveSnapshot <path>`); attaching another session replaces the agent's settings.

On that slice the changes above took the import from 695 s to 300 s on Linux; the same code takes about 270 s on an Apple silicon Mac, which is the baseline for further work on that machine. What is left in profiles is mostly the trampoline's own loop (about a sixth of the appender), name lookups (`visitRef`, `findUserFunction`), copying function bodies at every call, boxing the remaining complexity through `EvaluationResult[Int]`, and signature checks scripts make (`sigVerify`).

Tried and dropped, in the trampoline loop: keeping the current value and an error flag in two variables instead of a `Right` per `map` step, applying `map`/`flatMap` directly when the source is already a value (`Now`) instead of pushing and popping it, and ordering the loop's match by frequency. On a mainnet import past height 4.6M `EvaluationResult.run` stayed at 6% of the appender and script complexity per second did not change, so its share is most likely evaluator code inlined into it, not the loop. Beyond that the evaluator's design is the limit; PR #3792 rewrites it.

Profiles from JProfiler's async sampling on macOS are not reliable for proportions: they put 40 to 50% of the appender into the `write` syscall under RocksDB's WAL, where RocksDB's own timers measure 2 to 8%. Use Linux for profiles, and RocksDB statistics for the database share. Two indexes for name lookups were tried and measured no faster: a persistent `HashMap` updated on every block the evaluator enters, and an index of the top-level declarations built once per evaluation (267 s against 270 s on the slice). The `visitRef` share in profiles most likely includes the evaluation inlined into it, not just the walk over the enclosing blocks.
