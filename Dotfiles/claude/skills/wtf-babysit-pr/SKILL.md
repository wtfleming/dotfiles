---
name: wtf-babysit-pr
description: >-
  Watch an open pull request until it is ready for the user to merge — wait for CI
  and review bots on the current head, fix a failure the branch caused, work the
  review threads with wtf-address-review, push once per round, and repeat within
  a round budget. Ends when CI is green, every bot has looked at the head and
  nothing is left but decisions that are the user's. Use when asked to babysit,
  watch, shepherd or get a PR to green: "babysit #123", "watch this PR until it's
  ready", "/wtf-babysit-pr". Also the last step of wtf-ship-it. Never merges. NOT
  for a single pass over review comments (that is wtf-address-review).
argument-hint: "[PR number or URL — defaults to the current branch's PR] [--rounds N]"
---

Arguments: $ARGUMENTS

`--rounds N` is the budget, default 3; the rest is the PR. When it is empty, use the
current branch's: `gh pr view --json number -q .number`. If there is none, say so and stop.

# Babysit a pull request

The job is waiting, mostly. CI takes minutes, bots a few more, and each push restarts both.
So the loop is: wait for the head to settle, act on what it says, push once, and go round
again — never a push per fix, because each push starts a rate-limited bot review.

A **round** is one push. The budget caps rounds, not waits.

## 1. Wait for the head to settle

Wait for one commit, named up front: the PR's `headRefOid` on the first pass, and the commit
this skill just pushed (`git rev-parse HEAD`) on every pass after.

```sh
~/.claude/scripts/pr-wait.sh <n> <the commit>
```

It polls the status rollup of the PR's head and returns once that head is the named commit
and every check on it has finished, on two polls running. Tying the wait to the commit is
what makes "settled" mean this code: just after a push GitHub still serves the previous
commit's finished checks. Its exit status says how the wait ended, with one line of why:

- 0: settled. Go on.
- 1: timed out after 30 minutes. Report what the last poll saw and stop.
- 2: the repo runs no checks on this PR. Report CI as absent, not as passed.
- 3: `gh` itself failed on every poll for five minutes — auth, network, API — so CI is
  unknown. Report the error.
- 4: the head is some other commit, so someone else pushed. Start §1 again with the new
  head, and fast-forward to it before any edit (§2).
- 64: the script was called wrongly, or `gh` or `jq` is missing. Nothing was waited for:
  report the error and stop.

Run it with Bash `run_in_background` and wait for its notification. Then read
`gh pr checks <n> --json name,bucket,state,link` and the reviews on the head:

```sh
gh pr view <n> --json headRefOid,mergeable,mergeStateStatus,reviews
```

**A review bot has looked only if it reviewed this head.** A bot review whose commit is not
`headRefOid` is about older code. A bot check that passed, was cancelled or skipped can mean
*rate limited, did not look* rather than *found nothing* — read its output or its latest
summary comment before counting it. A bot that did not look is reported as that, never as
clean. A bot with no check of its own may post after CI settles: where one has reviewed
this PR before and has nothing on this head yet, poll the reviews for up to ten minutes
before reporting it did not look. A bot that says it is rate limited is not waited for or
re-triggered: report it as *did not look: rate limited* and go on. The same holds for a
bot that says it skips drafts: on a draft PR that is expected, so report *did not look:
draft* and do not wait. Human reviewers are not waited for: their review is the
user's to chase.

## 2. Act on what came back

Everything here edits the working tree and commits; nothing pushes until §3. So before
the first edit, the checkout must be the PR's head branch, clean, at `headRefOid`. If it is
another branch and clean, switch to the head branch; if it is clean and only behind —
someone committed a suggestion in the GitHub UI — `git fetch` and
`git merge --ff-only <headRefOid>`; if it is dirty or has diverged, stop and report rather
than committing somewhere the push cannot carry.

- **Conflicts with the base** (`mergeable: CONFLICTING`). Do not rebase or merge the base
  in — that rewrites a public branch or widens the diff. It is **Needs you**; go to §4.
- **A failed check.** Read why: `gh run view <run id> --log-failed`. If the branch caused it,
  fix it the way `~/.claude/reference/review-fix-round.md` steps 1–4 fix a finding —
  snapshot, edit, tests and lint, cold review of the fix diff — and commit. A check that
  fails the same way on the head an earlier round's fix for it produced is **Needs you**:
  that fix aimed at the wrong cause, and another push spends a bot review to learn it
  again. If the branch did not cause it —
  a runner outage, a flaky test the base also fails, an expired secret — and §2 commits
  nothing, re-run it once (`gh run rerun <run id> --failed`) and go back to §1 without
  counting a round; a second failure is **Needs you**. Where §2 does commit a fix, skip
  the rerun: §3's push starts every check again on the new head. A rerun returns before
  its checks exist, so until `gh run view <run id> --json attempt` shows the attempt
  number go up, §1 would settle on the old failure: wait for that first.
- **Open review threads or review bodies.** Follow
  `~/.claude/skills/wtf-address-review/SKILL.md` steps 1–4. At its step 5, make its
  commits, then push in §3 together with any CI fix, and post after that push the replies
  that name a commit this round made, as its step 5 says. The rest do not wait for a push
  that may never come: post a **Decline** or an **Answer** here, and an **Already
  addressed** too, resolving its thread, where the commit it names is already reachable
  from `headRefOid`. Every post, here or after the push, is subject to that skill's check
  before each post. Its step 1 skips what an earlier round already answered, by the marker
  each reply carries, so a declined thread is not answered again every round.

A Critical or Warning from a cold review here gets one repair before the push, checked
against the code like any finding, with a second cold review of that repair. Holding it
for the next round instead would spend a push on a fix already in hand. If the second
review still has a Critical or Warning, hold that fix out of the push, as
`wtf-address-review` holds a thread: report it, and do not go round again.

## 3. Push, then go round

If §2 committed anything: check the local branch is the PR's head branch and has not
diverged from `headRefOid`, push once, post the held-back replies, count a round, and go
back to §1. Once the budget is spent, §1 still runs for the head just pushed — the report
needs its results — but §2 commits nothing more.

If §2 committed nothing and started no re-run, the head is settled and nothing more will
happen without the user: go to §4.

**A push strands a published verification.** If the body has a `<!-- verify:start -->`
section, it now describes a tree that is gone; say so and name `wtf-code-verify` as what
would refresh it. Do not re-run verification here — it is the expensive step.

## 4. Report

**Ready** means all of: every required check passed on the head, every review bot looked at
the head or said why it did not (rate limited, a draft), no unresolved thread except
declined ones awaiting their reviewer, nothing under **Needs you**, and no conflicts.
Anything short of that names what is missing: a decision still owed is not ready.

```markdown
## PR #<n> — <ready to merge | not ready: <what is missing>>

**Head:** <short sha> · **Rounds:** <used>/<budget>
**CI:** <each check — result> · **Bots:** <each — looked at head: findings | did not look: why>
**Threads:** <fixed k · declined k · answered k · open k>

### Needs you
- <the decision, the options, and the thread or check it came from>
```

Where the user may have walked away — always, when this ran under `wtf-ship-it` — send the
headline as a PushNotification too. Then stop. Never merge, enable auto-merge or mark a
draft ready.
