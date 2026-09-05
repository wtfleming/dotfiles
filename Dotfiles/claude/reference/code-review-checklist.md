# Code Review Checklist

Read this before reviewing code or preparing a PR.

## Priority Order

Review in this order. Stop at critical issues.

### 1. Correctness (Critical)
- [ ] Logic errors, off-by-one, wrong operator
- [ ] Edge cases: null, empty, zero, negative, max values
- [ ] Async: race conditions, unhandled promises, missing await
- [ ] Data flow: correct inputs → correct outputs

### 2. Security (Critical)
Frame this pass with the OWASP Top 10 and CWE Top 25 categories — knowledge you
already have; don't fetch the documents. The lists are a floor, not a filter: a
security issue that fits neither list still gets reported.
- [ ] Input validation at system boundaries
- [ ] No hardcoded secrets or credentials
- [ ] SQL/command injection possible?
- [ ] Sensitive data exposure in logs/errors

### 3. Maintainability (Important)
- [ ] Code is readable without comments explaining it
- [ ] Names are clear and consistent with codebase
- [ ] Functions are focused (single responsibility)
- [ ] Error messages are actionable

### 4. Performance (Important)
- [ ] N+1 queries or unnecessary loops
- [ ] Resource leaks (connections, file handles)
- [ ] Blocking operations in async code
- [ ] Unbounded growth (memory, data structures)

### 5. Testing (Important)
- [ ] New code has tests (if repo has tests)
- [ ] Edge cases covered
- [ ] Tests are deterministic, not flaky

### 6. Dependencies & Deployment (Important)
- [ ] New dependency justified: necessary, maintained, reasonable transitive weight
- [ ] Breaking changes to public interfaces, config formats, CLI flags
- [ ] Migrations/rollouts reversible, or the irreversibility called out
- [ ] New failure paths observable (a log line or error someone can act on)

## Author's Finish Line

Run this over your own change before handing it to any reviewer, human or agent.
Everything here is cheaper not to write than to have someone else find.

- [ ] No debug code, commented-out blocks, backup copies or scratch files
- [ ] No path left dead by the change — the code you replaced is gone
- [ ] Every touched file is one the task required; nothing swept up in passing
- [ ] The commands you ran are reported exactly, with their results — not "tests pass"
- [ ] Assumptions stated, and anything you did not actually run said plainly

## Feedback Format

```
[CRITICAL] file.ts:42 - SQL injection via unsanitized input
[IMPORTANT] file.ts:78 - N+1 query in loop, use batch fetch
[MINOR] file.ts:12 - Naming: `data` → `userRecords` for clarity
```

## Rules

- Provide specific file:line references
- Explain the problem AND suggest a fix
- Critical blocks merge, Important should fix, Minor is optional
- Match existing code style, don't "improve" unrelated code
