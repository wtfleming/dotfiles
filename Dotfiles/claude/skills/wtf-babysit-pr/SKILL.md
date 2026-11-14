---
name: wtf-babysit-pr
description: >-
  Watch an open pull request until it is ready for the user to merge — wait for CI
  and review bots on the current head, fix a failure the branch caused, work the
  review threads with wtf-address-review, push once per round, and repeat within
  a round budget. Ends when CI is green, every bot has looked at the head and
  nothing is left but decisions that are the user's. Use when asked to babysit,
  watch, shepherd or get a PR to green: "babysit #123", "watch this PR until it's
  ready", "/wtf-babysit-pr". Also the last step of wtf-ship. Never merges. NOT
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

Take `headRefOid` and wait until every check on it has left `pending`. Wait in the
background with a deadline, so a hung check ends the wait rather than the session:

```sh
start=$(date +%s)
# Settled = gh answered, at least one check exists, none pending. Right after a push gh
# exits 1 with "no checks reported", which must read as "not yet", not as "done".
until b=$(gh pr checks <n> --json bucket -q '.[].bucket') && [ -n "$b" ] \
    && ! printf '%s\n' "$b" | grep -qx pending; do
  elapsed=$(( $(date +%s) - start ))
  [ -n "$b" ] || [ "$elapsed" -lt 300 ] || { echo "no checks after 5 min"; exit 2; }
  [ "$elapsed" -lt 1800 ] || { echo "timed out"; exit 1; }
  sleep 30
done
```

Exit 2 means the repo runs no checks on this PR: report CI as absent, not as passed.

Run it with Bash `run_in_background` and wait for its notification. On a timeout, report the
checks still pending and stop.

Then read `gh pr checks <n> --json name,bucket,state,link` and the reviews on the head:

```sh
gh pr view <n> --json headRefOid,mergeable,mergeStateStatus,reviews
```

**A review bot has looked only if it reviewed this head.** A bot review whose commit is not
`headRefOid` is about older code. A bot check that passed, was cancelled or skipped can mean
*rate limited, did not look* rather than *found nothing* — read its output or its latest
summary comment before counting it. A bot that did not look is reported as that, never as
clean. Human reviewers are not waited for: their review is the user's to chase.

## 2. Act on what came back

Everything here edits the working tree and commits; nothing pushes until §3.

- **Conflicts with the base** (`mergeable: CONFLICTING`). Do not rebase or merge the base
  in — that rewrites a public branch or widens the diff. It is **Needs you**; go to §4.
- **A failed check.** Read why: `gh run view <run id> --log-failed`. If the branch caused it,
  fix it the way `~/.claude/reference/review-fix-round.md` steps 1–4 fix a finding —
  snapshot, edit, tests and lint, cold review of the fix diff — and commit. If it did not —
  a runner outage, a flaky test the base also fails, an expired secret — re-run it once
  (`gh run rerun <run id> --failed`); a second failure is **Needs you**.
- **Open review threads or review bodies.** Follow `~/.claude/skills/wtf-address-review/`
  `SKILL.md` steps 1–4. At its step 5, make its commits, then push in §3 together with any CI
  fix, and post its replies after that push as its step 5 says.

A Critical or Warning from any cold review here holds that fix out of the push, as
`wtf-address-review` holds a thread: report it, do not loop on it within the round.

## 3. Push, then go round

If §2 committed anything: check the local branch is the PR's head branch and has not
diverged from `headRefOid`, push once, post the held-back replies, and count a round. If the
budget is spent, go to §4; otherwise go back to §1.

If §2 committed nothing, the head is settled and nothing more will happen without the user:
go to §4.

**A push strands a published verification.** If the body has a `<!-- verify:start -->`
section, it now describes a tree that is gone; say so and name `wtf-code-verify` as what
would refresh it. Do not re-run verification here — it is the expensive step.

## 4. Report

**Ready** means all of: every required check passed on the head, every review bot looked at
the head, no unresolved thread except declined ones awaiting their reviewer and **Needs
you**, and no conflicts. Anything short of that names what is missing.

```markdown
## PR #<n> — <ready to merge | not ready: <what is missing>>

**Head:** <short sha> · **Rounds:** <used>/<budget>
**CI:** <each check — result> · **Bots:** <each — looked at head: findings | did not look: why>
**Threads:** <fixed k · declined k · answered k · open k>

### Needs you
- <the decision, the options, and the thread or check it came from>
```

Where the user may have walked away — always, when this ran under `wtf-ship` — send the
headline as a PushNotification too. Then stop. Never merge, enable auto-merge or mark a
draft ready.
