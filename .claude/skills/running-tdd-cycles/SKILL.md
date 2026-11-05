---
name: running-tdd-cycles
description: Drive strict red-green-refactor TDD discipline on any code change in this repo.
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(sbt *), Bash(git status *), Bash(git diff *), Bash(git add *), Bash(git commit *)
---

## Loop

For each new piece of behaviour:

```
1. Extract <requirement>: the smallest piece of logic that adds value.
2. RED      -> write ONE failing test that pins down <requirement>.
3. GREEN    -> write the minimal code that makes it pass.
4. REFACTOR -> improve structure with the test as a safety net.
5. COMMIT   -> one logical change per commit (defer to committing-changes).
6. Repeat 2-5 until the task is done.
7. REVIEW   -> defer to reviewing-changes for the final pass.
```

Always one requirement per cycle. If the cycle feels big, the requirement was too big: split it.

## RED: write a failing test

- **One test, one requirement.** Don't write a test suite up front; one test, fail, pass, refactor, next.
- **Test names are claims, not labels.** Verify the fixture shape, input source, and assertion target match the name *before* the test goes green. A test that passes while pinning the wrong invariant is silently broken.
- **Arrange-Act-Assert.** Three blocks, one assertion focus. Name the behaviour: `"rejects a transfer with negative amount" in { ... }`.
- **Fail for the right reason.** Run the test before writing implementation: `sbt "node-tests/testOnly *FooSpec"`. The failure message must point at *missing behaviour*, not at a typo or fixture mistake.
- **No premature edge cases.** Happy path first; edge cases (empty collections, boundary values, error branches) get their own cycles.
- **Property-based when applicable.** Use `PropSpec` + `forAll` with ScalaCheck for invariants; example-based tests are for specific edge cases or documentation. See scala-conventions.
- **Reuse generators.** Check `node-testkit` (`TransactionGen`, `BlockGen`, ...) before writing a new `Gen`; add reusable ones back there.

## GREEN: minimal code to pass

- **Smallest possible change.** Hard-coded returns are fine for the first cycle; generalise only when a second test forces it.
- **No bonus features.** Don't add error handling, logging, or generality unless a test demands it.
- **Don't modify the test.** If the test is wrong, go back to RED. If the test is right and the implementation is wrong, fix the implementation.
- **Run the affected suite after each change**; run `sbt node-tests/test` before committing.

## REFACTOR: improve structure

- **Tests stay green throughout.** Run the suite after every micro-step.
- **Code smells to act on:** duplication (extract method), long methods (decompose), classes with a second responsibility (split), long parameter lists (case class), dead code (delete).
- **Tests refactor too.** Extract common fixtures into the testkit, rename for clarity, eliminate test duplication.
- **Performance refactors are measured** (module `benchmark`); commit the measurement alongside the change.

## Anti-patterns

- Writing implementation before the test.
- Writing a test that already passes.
- Writing many tests at once and implementing them in a batch.
- Modifying a test to make it pass.
- Skipping refactor because "the test passed."
- Test-after rationalised as TDD.

If the discipline breaks: stop, identify the violated phase, revert to the last green state, resume from the right phase.

## Cross-references

- `committing-changes`: commit after every successful GREEN or REFACTOR phase.
- `reviewing-changes`: final pass after the loop ends.
- `scala-conventions`: test framework, base specs, generators, run commands.
- `engineering-philosophy`: Small Steps, Investigate-Don't-Mask, KISS, YAGNI all apply directly.
