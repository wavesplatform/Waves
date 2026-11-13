---
purpose: Implementation notes for the RIDE evaluator (EvaluatorV2, EvaluationResult, Terms.copyForEvaluation) and how to measure its speed
---

# RIDE evaluator

## The evaluator rewrites the script tree in place

`EvaluatorV2` evaluates by writing results back into the tree: `FUNCTION_CALL.args`, `BLOCK.body`, `GETTER.expr`, `IF.cond` and `LET.value` are `var`s, and a reference to a `LET` replaces its value with the evaluated one, which is how LETs are lazy and evaluated once. So it must never run on the tree a script holds, which is shared between invocations and threads. `Terms.copyForEvaluation` makes the copy it runs on, and copies only what gets written into before the evaluator reaches it:

- Leaves (`REF`, evaluated constants, `FAILED_*`) are immutable and shared.
- Function declarations are shared: a user function call copies the function body (`evaluateUserFunction`) and evaluates the copy, never the body in place.
- A `LET` is a new node whose value is still the original's, marked with `LET.valueShared`. `evaluateRef` copies the value when it first evaluates it, so values nothing references are never copied. `valueShared` is a body field, not part of `LET`'s equality.

Anything that starts evaluating a tree, or adds a path that writes into one, has to keep to this: a node written into must have been copied first. In particular, never rebuild a `LET` (`copy`, `LET(name, value)`) between `copyForEvaluation` and its first evaluation: the new node drops `valueShared` while still holding the original value, and the evaluator would write into the shared tree. `EvaluatorV2Test` evaluates one tree twice and checks it is unchanged afterwards.

The old `EXPR.deepCopy` (through `cats.Eval`) is still there for the benchmarks and as the reference in `CopyForEvaluationTest`; it allocated several objects per node and copied whole dApps for every invocation.

## EvaluationResult is a trampoline, not Monix

`EvaluationResult` used to wrap `Coeval[Either[...]]`, so every evaluator step went through Monix's run loop and allocated a `Coeval` and an `Either`. It is now its own small trampoline: immutable step descriptions (`flatMap`, `map`, `transform`, `handleErrorWith`, `delay`, `defer`) run by `run()` with a stack local to the run. It keeps Coeval's behaviour where results depend on it: an error value skips the following `flatMap`/`map` steps, a thrown non-fatal exception unwinds to the nearest `handleErrorWith` or out of `run()`, and `applyLimitedCoeval` still returns a `Coeval` that turns an exception into `Left((e.getMessage, limit, log))`. Error texts of failed transactions end up in the state, so these paths must not change. `Extended` native functions (`invoke` and friends) still return a `Coeval`, run with `fromCoeval`.

An invoke between dApps evaluates the callee in a nested run on the same thread (the `Extended` function's `Coeval`, then a new `run()`), so the call stack grows with the depth of the chain, up to 100 invokes. A `StackOverflowError` is fatal and nothing catches it, so nodes with different stack sizes could disagree about a block. `SyncDAppLimits` runs the deepest allowed chain on a thread with a 1 MB stack to keep the margin visible.

Monix stack tracing (`monix.eval.stackTracingMode`, `cached` by default) records every `Coeval` and `Task` step to add async frames to exception traces, and script evaluation still enters `Coeval` for every invocation. `Application.main` turns it off unless the property is set.

## Measuring

Import a dApp-heavy stretch of testnet: import once to 2146124 (`waves import -i <blocks-file> -h 2146124`), keep a copy of that `data` directory, and time each variant importing from a fresh copy of it with `-h 2176100`. Over those 30k blocks 94% of block appending is `InvokeScriptTransactionDiff`. With the JProfiler agent started from a config file, take intermediate snapshots with `jpcontroller -n <pid>` (`saveSnapshot <path>`); attaching another session replaces the agent's settings.

On that slice the changes above took the import from 695 s to 300 s. What is left is mostly the trampoline's own loop, name lookups (`visitRef` and `findUserFunction` walk the enclosing blocks, which for a dApp include every top-level declaration), copying function bodies, and boxing the remaining complexity through `EvaluationResult[Int]`. A persistent `HashMap` index of the enclosing blocks was tried and cost as much as the walk it replaced: it is updated on every block the evaluator enters, including the argument blocks of every user function call.
