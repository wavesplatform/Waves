---
description: Four-pass quality gate (code, security, architecture, acceptance) via parallel independent reviewers.
allowed-tools: Bash(git diff *), Bash(git log *), Bash(git rev-parse *), Bash(gh pr view *), Bash(gh pr diff *), Bash(gh issue view *)
---

Scope: $ARGUMENTS

If `$ARGUMENTS` is empty, default the scope to `git diff origin/version-1.6.x...HEAD`. If it looks like a PR number, resolve it via `gh pr diff <N>`. Otherwise pass it through verbatim as the diff range.

Launch the four review agents **in parallel**, a single message with four Agent tool calls, passing each only the scope (diff range or PR number), nothing from the current conversation:

1. `@code-reviewer`: Pass 1, code quality
2. `@security-auditor`: Pass 2, security audit
3. `@architect-review`: Pass 3, architecture consistency
4. `@acceptance-auditor`: Pass 4, intent alignment (does the diff solve the linked issue?)

Each agent returns a verdict line (`PASS / NEEDS WORK / FAIL`) plus its findings.

Aggregate the reports into one Quality Gate Summary table:

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

Then list every Critical and Major finding from all passes with `Rule / Severity / Location / Issue / Fix`. Skip Minor unless the overall verdict is PASS (then include them as polish).

If parallel agents are unavailable in the current harness, fall back to invoking the `reviewing-changes` skill directly: same passes, one inline context.
