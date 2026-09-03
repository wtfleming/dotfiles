# Resolving a scope nobody named

When a review, verification or design pass is invoked without a scope, it has to work one
out. Getting this wrong is quiet rather than loud: the commands below can return an empty
result and exit 0, and an empty result read as "no changes" produces a confident report
about nothing.

This file is the single statement of the procedure. `wtf-code-verify`, `wtf-change-reviewer`,
`wtf-design-reviewer` and `wtf-code-review` all point here rather than restating it, so a
fix lands once.

## The order

1. **Uncommitted work**, if there is any:

   ```sh
   git status --porcelain
   git diff                                    # unstaged
   git diff --staged                           # staged
   git ls-files --others --exclude-standard    # untracked, which diff never lists
   ```

   The last one matters. A new source file beside a Markdown edit is exactly the change
   that would otherwise pass as prose, and `git diff` does not mention it.

2. **The branch against its merge base**, if the tree is clean.

3. **`git show HEAD`**, if that is empty too.

## Resolve the default branch; do not hardcode `main`

```sh
# Take the first candidate that resolves to a commit. Validating the result rather than
# trusting the source is the whole point: origin/HEAD can be a dangling symbolic ref
# after the upstream default branch is renamed, a local name can be missing in a fresh
# clone, and the prefix can be stripped off a ref that needed it. All three produce a
# base that looks resolved and is not.
base=""
for c in "$(git symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null)" \
         origin/main origin/master origin/trunk main master trunk; do
  [ -n "$c" ] || continue
  git rev-parse --verify --quiet "$c^{commit}" >/dev/null 2>&1 && base="$c" && break
done
[ -n "$base" ] || echo "cannot resolve a default branch; ask for one" >&2
```

Two things that snippet is doing deliberately. Remote-tracking refs come before local
names because they are the ones that exist in a clone nobody has branched in. And every
candidate goes through `rev-parse --verify` including the one `origin/HEAD` names — a
symbolic ref is a pointer, and nothing guarantees its target still exists.

On a `master` repo, `git merge-base HEAD main` fails, a `$(...)` substitution collapses to
empty, and `git diff ...HEAD` degrades to `HEAD...HEAD` — empty output, exit 0, no stderr
anyone reads. That is the failure this section exists to stop, and it looks identical to a
clean tree.

If none of them resolves — `origin/HEAD` unset and the default branch named something else
— **say so and ask, rather than guessing.** Falling through to `git show HEAD` there
reviews a single commit of a branch that has many, and reports it as the whole scope.

## An empty diff means fall through, not "no changes"

Two ordinary situations produce an empty merge-base diff with exit 0: standing on the
default branch itself, and the collapsed substitution above. Neither means the branch has
no changes. Treat empty as *this step found nothing, continue to the next* — never as a
verified scope of zero.

## State what you settled on

Whatever the procedure lands on, name it at the top of the report: the ref, the file count,
and which of the three steps produced it. A reader who cannot see the choice has no way to
tell whether the report covers the code they meant.
