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
base=$(git symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null | sed 's|^origin/||')
[ -n "$base" ] || for c in main master trunk; do
  git rev-parse --verify --quiet "$c" >/dev/null && base=$c && break
done
```

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
