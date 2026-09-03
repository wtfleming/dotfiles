# Deriving expectations

An expectation is a falsifiable pair: an input or action, and the observable it should
produce. Getting these right is most of the work — a probe built on the wrong idea of
correct will run cleanly, pass, and tell you nothing.

## Contents

- Where expectations come from
- The tautology trap
- The negative-case catalogue
- How it fails, not just that it does
- Verifying prose against code
- Verifying a PR's title and description
- Verifying a subject with no diff
- How many expectations

## Where expectations come from

In rough order of authority, because each of these was written by someone who knew
something you are trying to reconstruct:

1. **The PR body, ticket or commit message.** The author already wrote down what they
   think they did. Start here and treat it as a claim to be tested, not a fact.
2. **The existing tests.** What the suite already asserts *is* the intended contract.
   A test that pins the old behaviour and was edited in this change is the single most
   informative thing in the diff: someone decided the contract should move.
3. **The schema or types.** What is required, what is nullable, what the enum permits,
   what the response is declared to contain. GraphQL SDL, OpenAPI, migrations, structs.
4. **Validation code and error messages.** The author's own list of what to refuse.
5. **Docs, README, ADRs, comments** — with the caveat below that these are claims too.

## The tautology trap

The most common way an expectation goes wrong is being read off the implementation it is
about to test. If you read `if (!user) return 401` and write the expectation "anonymous
caller gets 401", you have verified that the code does what the code does. It will pass.
It would also pass if 401 were the wrong answer.

Take the expectation from somewhere outside the implementation: the test that already
exists, the schema, the documented contract, the caller that depends on it, or plain
reasoning about what an endpoint of this kind must do to be correct. Where no external
source exists and the expectation is genuinely your own inference, say so in the report
and treat it as weaker evidence — an inference confirmed is worth less than a contract
upheld, and the reader cannot tell them apart unless you separate them.

## The negative-case catalogue

The positive case was always going to pass; it is the one the author ran by hand. Bugs
live in the cases below. Pick the ones the change's surface actually has — this is a
menu, not a checklist to exhaust.

**Input crossing a boundary**

- absent, empty, and null — three distinct inputs, and code routinely handles two of them
- wrong type, wrong shape (an array where an object belongs, a nested value flattened)
- boundary values: `0`, `-1`, exactly the limit, one past the limit
- too long, too many, past the pagination cap
- an unknown or extra field — rejected or silently ignored? Both can be right, and the
  change decides which; the bug is when nobody decided
- encoding: unicode, emoji, a newline, leading and trailing whitespace
- injection-shaped input: a quote, `<script>`, `../`, a null byte. Not primarily to hunt
  a vulnerability, but because the escaping path is where the 500s are

**Authorisation** — the highest-value negatives, and the ones an author is least likely
to have tried by hand

- no credential at all
- a malformed credential
- an **expired** credential, which is a different code path from a missing one and the
  classic gap
- a valid credential belonging to **another user** — can A read or write B's object? This
  is the most commonly missed case in the catalogue and the most expensive to miss
- a valid credential with insufficient role or scope
- a revoked or logged-out credential still presented
- and whether the refusal leaks existence: `404` and `403` tell the caller different
  things about whether the object is there

**State and persistence**

- the same request twice — a duplicate row, a `409`, or a silent overwrite?
- two at once, if concurrency is plausible
- a partial failure midway: does the write roll back, or leave half a record?
- a request that arrives before its prerequisite

**Dependencies**

- the dependency down, slow, or returning garbage. Does it fail **open** or **closed**?
  An auth check that fails open is a security hole; a cache that fails closed is an
  outage. The change has to pick one, and the expectation names which it picked.

**Scale** — review can spot an N+1 by reading it; what running adds is the number

- the request against a realistic row count, not the ten rows in the fixture
- **query count per request, compared across the differential.** A list endpoint that
  issued 3 queries and now issues 3 + N has a loop in it, and the count says so plainly
  where a wall-clock reading is noise
- a response with no upper bound: what comes back when the collection has 10,000
  members, and does anything page it?
- work newly done *inside* a loop — a network call, a file read, a serialization
- memory across a long run, where the change holds something it used to release

This lives here rather than under the performance variant because nobody claimed
anything. A performance claim gets measured because someone made it; an accidental
regression ships because every correctness probe passed on ten rows.

## How it fails, not just that it does

An expectation that says only "it should error" cannot be falsified by the most common
real defect, which is erroring *wrongly*. Name the shape of the refusal:

- the right status or exit code — `400` for a bad request, not `500`; `401` for absent
  credentials, `403` for insufficient ones
- a message a human can act on, naming the field or the constraint
- no stack trace, no SQL, no internal path, no secret in the response body
- nothing written on the failure path — no orphaned row, no partial file, no sent email
- logged at a level that matches: a user's typo is not an error-level event, and a
  failing dependency is not a debug one

**Could you debug it from outside?** You have already run the negative case, so this
costs one look at the log. Read only what an on-call engineer would have — the log line,
the metric, the error-tracker event — and ask whether it names the input, the caller and
the operation. `Error: undefined` with no correlation id means the code is correct and
the next incident is going to be a long one. It is the only part of the failure path that
nobody exercises until the moment they need it.

## Verifying prose against code

For documentation, comments, READMEs and PR descriptions, the polarity inverts: **the
code is the source of truth and the prose is the subject under test.** Docs cannot be
"run", so verification means extracting each claim and checking it individually.

Extract claims with `file:line`, then classify each — the class decides how it is
checked:

| Claim class | Example | How to verify |
| ----------- | ------- | ------------- |
| a symbol or signature exists | "pass `meterName:` to the constructor" | grep the source; a documented parameter that does not exist is the classic finding |
| behaviour | "returns 404 when the slug is unknown" | run it |
| quantitative | "retries three times", "defaults to 30 seconds" | find the constant, and prefer running it where the constant is derived |
| procedural | "run `make setup`, then `make dev`" | actually run it, in a clean worktree — which is what makes the worktree worth building for a docs change |
| unfalsifiable | "this module is responsible for orchestration" | skip it, and say you skipped it |

Three pitfalls specific to prose:

- **Do not verify a claim by re-reading the prose.** Internal consistency across two
  paragraphs is not accuracy, and a confidently written wrong claim reads as more
  reliable than a hedged right one.
- **A name that looks right is not a name that exists.** Plausible-but-absent symbols are
  what documentation drift produces most, and they are invisible to a reader who is not
  checking each one against the tree.
- **Stale and wrong are indistinguishable to the reader** who follows the instruction. A
  claim that was true two releases ago is a defect now; date it if you like, but report
  it either way.

The highest-value finding here is the claim that is *silently* wrong — accurate-sounding,
plausible, and producing no error when followed, just the wrong result.

## Verifying a PR's title and description

A description is written when the PR opens and then quietly stops being true. Review
lands, commits follow, and the body still describes the change as first proposed — which
is the version a future reader believes, because a squash merge turns the title into the
permanent commit subject on `main` and the body into the message beneath it.

Check it whenever the scope resolves to a PR. It is cheap: no environment, no worktree,
and only the behavioural claims need a probe at all.

**Two directions, and the second is the one that drifts.**

*Claims to code* — each falsifiable statement in the body, checked as any prose claim is:
"adds a `--dry-run` flag", "no migration needed", "behind the `archive` flag".

*Code to claims* — each meaningful change in the diff, checked for whether the body
accounts for it. Omission is what review produces: a reviewer asks for null handling, a
commit adds it, nobody edits the body. Nothing in the body is false, so a claim-by-claim
pass reports clean, and the description is wrong anyway — by silence. This direction is
the whole reason to run the check.

**Go straight to what arrived after it opened.** You do not need to re-verify the whole
body, only the part the post-open commits touched.

```bash
gh pr view <n> --json title,body,createdAt,commits,reviews,comments
gh pr diff <n>                                     # the change as it stands
```

Find the boundary from `createdAt` against each commit's `committedDate`, both of which
that first call already returns, and diff from the last commit at or before it. Do not
reach for `<first-commit-of-pr>..HEAD`: `A..HEAD` excludes only `A` and its ancestors, so
a PR opened with three commits reports two of them as post-open work, and after a rebase
the old first commit is not an ancestor at all and you get the whole branch. It
over-includes rather than under-includes, so no verdict goes wrong — you just lose the
shortcut this section exists to give you.

The review threads are the map. A comment asking for a change, plus a commit answering
it, is a behaviour the body almost certainly does not mention — start there rather than
re-reading the diff from scratch.

**The title travels further than the body.** On a squash merge it becomes the subject
line on `main`, outliving the PR, the branch and the review. Where the repo derives
anything from it — a conventional-commit prefix, a changelog section, a release version
— a `fix:` that has grown into a `feat:` is not cosmetic. Judge the prefix separately
from the wording, and say which one moved.

**What a rewrite must not touch** — `Closes #123`, checklists, screenshots, template
sections. The rule and its consequences live in `evidence.md`, beside the `gh pr edit`
that does the rewriting.

## Verifying a subject with no diff

`/wtf-code-verify login and authentication` has no diff, no author and no claim. You have
to derive the contract first, from the sources above, and then test that contract —
including its refusals, which for a subject like authentication are most of the contract.

Say plainly which files you settled on and how you found them, and separate what you
verified against an external contract from what you verified against your own inference.
Without that split the report reads as though the whole area was pinned down, when part
of it was you agreeing with yourself.

## How many expectations

One per meaningful area of the change, not one per change. A five-file diff verified by a
single probe leaves four areas unexercised while the report reads as if it covered them.

Keep it affordable by picking the cheapest tier that can see each claim rather than by
cutting the list — several tier-0 probes cost less than one tier-2 probe and cover more.
When the list genuinely runs past the budget, name the claims you dropped and why.
An honest three-of-six is useful; a silent three-of-six is misleading.
