---
name: reviewing-changes
description: Four-pass review of a diff: code, security, architecture, acceptance.
allowed-tools: Read, Grep, Glob, Bash(git diff *), Bash(git log *), Bash(git show *), Bash(git status *), Bash(git rev-parse *), Bash(gh pr view *), Bash(gh pr diff *), Bash(gh pr list *), Bash(sbt *)
---

## Process

Run the four passes in order; findings flow into one combined verdict.

### 1. Read the rules

Before any pass, internalise `scala-conventions` and `engineering-philosophy`. They are the source of truth; don't invent additional standards.

### 2. See the change

```
git diff origin/version-1.6.x...HEAD
git log origin/version-1.6.x..HEAD --oneline
```

For a GitHub PR: `gh pr view <N>`, `gh pr diff <N>`.

### 3. Pass 1: Code quality

Check, in order:

- **Philosophy violations**: over-engineering (KISS, YAGNI), duplication (DRY), magic behaviour, copy-paste-modified blocks.
- **Redundant entity & local-pattern reuse**: when the diff adds a function, accessor, or generator the repo already provides (testkit generators, cats syntax, an idiom a sibling file implements), reuse the existing form instead of a second variant. DRY spans the whole repo, not just this diff.
- **SOLID violations**: Single Responsibility first; flag classes/files that grew a second responsibility.
- **Naming, readability, complexity**: method lengths, parameter lists, deeply nested pattern matches, clever one-liners that hide intent.
- **Test coverage**: was the change tested? If TDD discipline applied, was the failing test committed first?
- **Tooling compliance**: `sbt compilePR` passes (scalafmtCheck + `-Werror` compile); no new `-Wconf` suppressions without a stated reason; no edits to `src_managed` output.
- **Configuration safety**: timeouts, pool sizes, retry behaviour on network paths.

### 4. Pass 2: Security audit

Check, in order:

- **Input validation**: every externally supplied value (REST/gRPC params, network messages, config) validated before use; deserialization bounds-checked.
- **Consensus & validation invariants**: no weakening of transaction/block validation, no non-deterministic code (system time, iteration order, floating point) on consensus paths.
- **Wire/storage format changes**: any change to serialized layouts must be versioned, never silent; protobuf field numbers are append-only.
- **Sensitive data exposure**: private keys and seeds never logged, serialized, or exposed via API; no secrets in error messages.
- **Cryptographic issues**: use the pinned crypto providers from `Dependencies.scala`; no hand-rolled primitives; no predictable randomness (`scala.util.Random`) where security matters.
- **Resource exhaustion**: unbounded collections from network input, missing size limits, quadratic parsing on attacker-controlled data.
- **Vulnerable components**: new/updated deps checked for known CVEs; version pins stay in `Dependencies.scala`.

### 5. Pass 3: Architecture consistency

- **Module boundaries**: does the change respect the sbt module graph (`node` core, `node-testkit` fixtures, `grpc-server` transport, `node-it` integration)? Test helpers belong in `node-testkit`, not copied per suite.
- **Layer violations**: dependencies pointing the wrong way; API/transport layers reaching into storage internals.
- **Missing abstractions**: same logic implemented twice with minor variations.
- **Custom code where a library exists**: presumptive Critical when the diff reinvents primitives already in the dependency tree (crypto, codecs, retry, HTTP). If `Dependencies.scala` already pulls in a library exporting the function being hand-rolled, that is Critical regardless of LoC.

### 6. Pass 4: Acceptance / intent alignment

Does the diff actually solve the contract (linked GitHub issue or PR description)? Three axes:

- **Drift**: implements something related but not the asked feature.
- **Partial**: covers some required behaviours but misses others.
- **Overreach**: includes changes the issue did not request.

## Output

```
## Quality Gate Summary

| Review       | Verdict        | Critical | Major | Minor |
|--------------|----------------|----------|-------|-------|
| Code         | pass/warn/fail | N        | N     | N     |
| Security     | pass/warn/fail | N        | N     | N     |
| Architecture | pass/warn/fail | N        | N     | N     |
| Acceptance   | pass/warn/fail | N        | N     | N     |

**Overall**: PASS / NEEDS WORK / FAIL

### Action items
1. <Critical/Major items, ordered>
```

For each finding: **Rule** (skill reference or "best practice"), **Severity** (Critical / Major / Minor), **Location** (`file:line`), **Issue** (what's wrong; for security, the attack vector), **Fix** (concrete suggestion).

## Behavioural traits

- Constructive, educational tone. Teach; don't just flag.
- Specific, actionable feedback. "This is too complex" without a fix is useless.
- Severity matches reality: Critical for "this could ship a bug or a CVE today"; Major for "this will hurt within six months"; Minor for style and polish.
- Practical over theoretical security risks.
- Read-only: this skill never edits the diff itself; it reports.

## Cross-references

- `running-tdd-cycles`: preceding workflow; review confirms TDD discipline.
- `committing-changes`: commit-message + branch hygiene checks fold into the code-quality pass.
- `scala-conventions` / `engineering-philosophy`: the rules the diff is checked against.
