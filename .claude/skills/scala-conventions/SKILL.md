---
name: scala-conventions
description: Apply this repo's Scala conventions: Scala 3, sbt, scalafmt, -Werror PR gate, ScalaTest + ScalaCheck via testkit base specs.
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(sbt *), Bash(git status *), Bash(git diff *)
---

## Stack

- **Scala 3** (`scalaVersion` in `build.sbt`, currently 3.8.4) on **JDK 17** (temurin in CI). No Scala 2 syntax in new code; use Scala 3 idioms (top-level definitions, `enum`, `given`/`using`, `extension`) where they simplify, but match the style of the file being edited.
- **sbt** multi-module build; module graph and PR commands live in `build.sbt` (`compilePR`, `checkPR`). Never invent parallel build entry points.
- **scalafmt** (`.scalafmt.conf`: version pinned, `maxColumn = 150`, `defaultWithAlign`). Format with `sbt scalafmt`; CI enforces `scalafmtCheck` on `Compile` sources only, inside `compilePR`, and test sources are not formatted, so `scalafmtAll` would rewrite untouched test files. Never hand-format to satisfy the checker.
- **Compiler is the linter.** `-Wunused:all`, `-feature`, `-deprecation`, `-unchecked` are always on; PR commands add `-Werror`, so any warning fails CI. Fix the code, don't widen `-Wconf` suppressions; a new `-Wconf` filter is a build-config change that needs a stated reason.
- **scalapb** generates protobuf sources into `src_managed`; never edit generated code, never copy generated symbols into hand-written files.

## Testing

- **ScalaTest + ScalaCheck.** Unit tests live in `node/tests` (module `node-tests`), shared fixtures and generators in `node/testkit` (module `node-testkit`).
- **Extend the testkit base classes**, not raw ScalaTest: `com.wavesplatform.test.{FreeSpec, PropSpec, FlatSpec, FunSuite, FeatureSpec}`. They mix in `BaseSuite` (Matchers, `ScalaCheckPropertyChecks`, `ShrinkLowPriority`, `TransactionGen`, `EitherMatchers`, `EitherValues`, `OptionValues`, logging). Bypassing `BaseSuite` silently loses the shrink policy and shared generators.
- **Style choice**: `FreeSpec` for behaviour trees (`"validation" - { "rejects X" in { ... } }`), `PropSpec` + `forAll` for property-based invariants, `FlatSpec`/`FunSuite` only when matching an existing sibling suite.
- **Generators are shared.** Before writing a new `Gen`, check `node-testkit` (`TransactionGen`, `BlockGen`, `DomainPresets`, ...); add reusable generators there, not in the test file.
- **Run**: `sbt node-tests/testFull` for the suite (plain `test` is incremental in sbt 2 and can pass having run nothing), `sbt "node-tests/testOnly *FooSpec"` for one class, `sbt grpc-server/testFull` for gRPC. Integration tests (`node-it`) need Docker and are run explicitly, never as part of a routine change.
- Test names are claims: name the behaviour (`"rejects a transfer with negative amount"`), not the method under test.

## Layout and idioms

- Follow the existing package structure under `com.wavesplatform`; new files go next to their closest sibling, not into new top-level packages.
- **Errors as values**: this codebase returns `Either[ValidationError, T]` on validation paths; keep that shape, don't introduce exceptions for control flow. Use cats syntax already in scope (`.leftMap`, `.flatMap`, `traverse`) rather than manual pattern-match plumbing.
- Dependency versions live only in `project/Dependencies.scala` (with `overrides` for transitive pins); never inline a version in `build.sbt` or a module.
- Concurrency uses the existing monix `Task`/`Observable` machinery; don't mix in a second effect system.

## Scratch testing

Quick exploration goes through `sbt node/Test/console` (REPL with test classpath) or a gitignored scratch file; never inline Scala via heredocs, never leave exploration code in a test suite.
