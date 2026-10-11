---
name: orca-integrate
description: >-
  Reintegrate an Orca-managed worktree into its parent branch or repo trunk locally,
  with no PR: squash or fast-forward the worktree branch, remove the Orca worktree,
  and delete the branch. Use when the user says "integrate", "land", "fold back",
  "reintegrate worktree", "merge worktree to trunk", or "$orca-integrate". Runs a
  bundled script; always dry-run first and show the user before executing.
---

# Orca Integrate

Fold an Orca-managed child worktree back into a target branch on the local machine —
no remote push, no PR. The bundled script owns the deterministic and risky git/orca
mechanics; you supply judgment (the commit message, conflict resolution) and confirm
the plan with the user first.

Script: `scripts/orca-integrate.sh` (relative to this skill's base directory), also on
`PATH` as `orca-integrate`.

## When to use / not use

Use it for the local trunk-based flow: a worktree was spun out for an agent task and
the work is finished, so it should be folded back into its parent branch or the repo
trunk.

Do **not** use it when:

- The repo's flow is PR-driven — open a PR instead.
- The work is not verified or not done yet. Integrate finished work.
- An agent is still working in the worktree. The script aborts on live terminals;
  confirm the worktree is idle before running.

## Workflow

1. **Confirm the work is complete** and that any agent in the worktree is idle. If
   it is unclear whether the worktree is done, ask the user before proceeding.
2. **Dry-run and show the plan.** Run the script with `--dry-run` and show the user
   the resolved worktree, target branch, mode, commit message, and the exact git/orca
   commands it would run. Get their sign-off.
   ```sh
   scripts/orca-integrate.sh <worktree-selector> --dry-run [options]
   ```
3. **Run for real** once the user approves. Remove `--dry-run` and pass the same
   options.
4. **On rebase conflicts (`--ff` only):** the script stops and leaves the rebase in
   progress. Resolve conflicts in the worktree with normal judgment, run
   `git rebase --continue`, then re-run the script. (Squash conflicts abort and reset
   the target checkout; handle those by hand if they occur.)
5. **Report the outcome.** State which branch advanced, whether the worktree and
   branch were removed, and anything left behind. When it is relevant, suggest updating
   the workspace card:
   ```sh
   orca worktree set --worktree <selector> --comment "…" --workspace-status completed --json
   ```

## Crafting the commit message

For `--squash` (the default), the derived message is only a fallback
(`<branch>: integrate worktree work`). This is your main value-add: read the
worktree branch's diff and log

```sh
git -C <worktree-path> log --oneline <target>..<branch>
git -C <worktree-path> diff <target>...<branch>
```

and pass a proper conventional message via `--message` that describes the change.

## Decision guidance

- **`--squash`** (default): best for agent WIP and multi-commit branches — one clean
  commit on the target.
- **`--ff`**: when the user wants granular history preserved. If the target has
  diverged, the script rebases the worktree branch onto the target first, then
  fast-forwards.
- **`--into <branch>`**: when the Orca parent branch is not the desired landing spot
  (e.g. land on trunk instead of a feature branch).
- **`--keep`**: integrate but leave the worktree and branch in place.
- **`--wip`**: the worktree is dirty and the user wants everything committed as
  `wip: <branch>` before integrating.

Target-branch auto-detection, in order: `--into` → the Orca parent worktree's branch
→ the worktree's recorded `baseRef` → repo trunk (`main`/`master`). Pass `--into` when
unsure, and show the detected target in the dry-run so the user can correct it.

## Hard rules

- **Never run the destructive steps outside the script.** Do not hand-run
  `git merge`, `orca worktree rm`, or `git branch -D` for this flow; let the script
  keep Orca's state and git's refs consistent.
- **Never skip the dry-run** unless the user explicitly says to.
- **Resolve selectors only from `orca … --json` output** — never guess a worktree id
  or path. Worktree ids are two-part (`<repoId>::<absolutePath>`); copy them verbatim.
- **Never push to a remote.** This is local reintegration only.
- The script aborts on a dirty worktree, live terminals, a dirty target checkout, or a
  missing target branch — before it changes anything.

## Script reference

```
orca-integrate.sh <worktree-selector> [--squash|--ff] [--into <branch>]
                  [--message "<msg>"] [--keep] [--wip] [--dry-run]
```

`--dry-run` executes only read-only discovery (so it can resolve and print the plan)
and makes no changes.
