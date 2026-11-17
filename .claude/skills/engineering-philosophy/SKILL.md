---
name: engineering-philosophy
description: Apply KISS, YAGNI, DRY, SOLID, fail-fast, be-brief on every code decision.
---

## Principles

- **KISS**: Simple solutions over complex ones.
- **YAGNI**: Build only what's needed now. Less code is better.
- **Write Less**: If you can avoid writing the code or the comment, don't. A comment earns its place only when it says what the code cannot: a why, an invariant, a magic-number derivation, never a restatement. **Default every comment to one line.** Multi-line is allowed ONLY for a byte-layout, a derivation, or a non-obvious why that needs the room. This covers every comment, source, config, and CI YAML alike; before committing, re-read each comment you added and cut it to one line or delete it.
- **DRY**: Single source of truth. Never copy-paste. Reuse spans the repo: prefer an existing sibling idiom over a second variant.
- **SOLID**: Enforce Single Responsibility; keep the others in mind when possible.
- **No Magic**: Make everything explicit. No hidden behaviour or implicit transformations.
- **No Number Without Measurement**: Performance figures in docs (latency, throughput, sizes) MUST come from a real measurement: a test run, a profile, a fixture, or an upstream spec citation. Unmeasured is hope, not fact.
- **Small Steps**: Minimal changes, commit often.
- **Stay In Scope**: Change only what the task requires. Don't fix, reformat, or rename unrelated code you happen to read, even when it looks wrong: note it and leave it. Off-task diffs are harder to review and revert; genuine cleanups earn their own PR.
- **Use Libraries**: Prefer established libraries over reimplementing features. Check `project/Dependencies.scala` and the ecosystem before writing custom code.
- **Backwards Compatibility**: Don't keep code for backwards-compatibility purposes (consensus and wire formats are the exception: they are versioned contracts, never silently changed).
- **CI**: Automate all possible quality checks.
- **Investigate, Don't Mask**: When a check fails or unexpected behaviour occurs, investigate the root cause instead of adding defensive code to mask the symptom.
- **Fail Fast**: Detect and surface errors immediately at the point of failure. Use strict validation and early returns.
- **Be Brief**: Imperative output. No preamble, no recap, no restating the task back. Compress *response prose*, never *operational checklists*: keep every named rule, severity word, sub-check, and category from the active skill verbatim. Applies to chat replies, commit messages, PR bodies, and review findings.

## Application

These principles are *judgement weights*, not rules. When two conflict, the workflow skill driving the task decides:

- During design: KISS, YAGNI, Use Libraries, and No Magic dominate. Reject premature abstractions and speculative configurability.
- During `reviewing-changes`: SOLID, DRY, Investigate-Don't-Mask, Fail Fast, and Stay In Scope dominate.
- During `running-tdd-cycles`: Small Steps, Stay In Scope, and Fail Fast dominate. One requirement per red-green-refactor; one logical change per commit.

When a user proposes a change that violates one of these principles, name the principle and explain the consequence: don't just refuse.
