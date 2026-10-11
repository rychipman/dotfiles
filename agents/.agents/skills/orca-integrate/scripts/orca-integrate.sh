#!/usr/bin/env bash
#
# orca-integrate.sh — reintegrate an Orca-managed worktree into a target branch,
# locally. No remote push, no PR flow.
#
# Usage:
#   orca-integrate.sh <worktree-selector> [--squash|--ff] [--into <branch>]
#                     [--message "<msg>"] [--keep] [--wip] [--dry-run]
#
# Defaults: --squash, cleanup enabled, target branch auto-detected.
#
# The script owns the deterministic/risky git + orca mechanics. The agent (or
# user) supplies judgment: the commit message and any conflict resolution.
# Always run with --dry-run first and review the plan before executing.
#
# Read ../SKILL.md for the workflow and gates.

set -euo pipefail

PROG=$(basename "$0")

# The Orca CLI. ORCA_CLI_COMMAND is exported by Orca for managed WSL sessions;
# on this macOS host the plain `orca` executable is correct.
ORCA_BIN="${ORCA_CLI_COMMAND:-orca}"

DRY_RUN=false
MODE=squash
INTO=""
MESSAGE=""
KEEP=false
WIP=false
SELECTOR=""

die()  { printf '%s: error: %s\n' "$PROG" "$*" >&2; exit 1; }
info() { printf '%s\n' "$*"; }
warn() { printf '%s: warning: %s\n' "$PROG" "$*" >&2; }

usage() {
  cat >&2 <<EOF
Usage: $PROG <worktree-selector> [--squash|--ff] [--into <branch>]
                   [--message "<msg>"] [--keep] [--wip] [--dry-run]

Reintegrate an Orca-managed worktree into a target branch, locally.

  <worktree-selector>  Orca worktree selector (name:<n>, path:<p>, id:<repo>::<path>,
                       branch:<b>, or active/current)
  --squash             squash-merge the worktree branch into the target (default)
  --ff                 rebase if needed, then fast-forward the target
  --into <branch>      target branch (default: parent worktree branch, then the
                       worktree's recorded baseRef, then trunk main/master)
  --message "<msg>"    commit message for --squash (default: "<branch>: integrate worktree work")
  --keep               keep the Orca worktree and branch (skip cleanup)
  --wip                commit a dirty worktree as "wip: <branch>" before integrating
  --dry-run            print the git/orca commands that would run; make no changes
  -h, --help           show this help

Always run with --dry-run first.
EOF
}

# --- helpers -----------------------------------------------------------------

# run: execute a mutating command; in dry-run, print it instead. Preserves the
# command's exit status in real mode so callers can `run ... || die ...`.
run() {
  if $DRY_RUN; then printf '  [dry-run] %s\n' "$*"; return 0; fi
  "$@"
}

# run_orca: mutating orca call; verifies the JSON envelope's .ok, since `orca`
# exits 0 even on failure. Returns non-zero on failure.
run_orca() {
  if $DRY_RUN; then printf '  [dry-run] %s\n' "$*"; return 0; fi
  local out
  if ! out=$("$@" 2>&1); then printf '%s\n' "$out" >&2; return 1; fi
  if ! jq -e '.ok == true' <<<"$out" >/dev/null 2>&1; then
    printf '%s\n' "$out" >&2
    return 1
  fi
  return 0
}

# orca_probe: read-only orca call; prints JSON, dies on failure.
orca_probe() {
  local out
  if ! out=$("$ORCA_BIN" "$@" 2>&1); then printf '%s\n' "$out" >&2; die "orca $* failed"; fi
  if ! jq -e '.ok == true' <<<"$out" >/dev/null 2>&1; then
    printf '%s\n' "$out" >&2
    die "orca $* failed"
  fi
  printf '%s\n' "$out"
}

# orca_maybe: read-only orca call that returns non-zero instead of dying.
orca_maybe() {
  local out
  out=$("$ORCA_BIN" "$@" 2>/dev/null) || return 1
  jq -e '.ok == true' <<<"$out" >/dev/null 2>&1 || return 1
  printf '%s\n' "$out"
}

branch_exists() { git -C "$SRC_PATH" show-ref --verify --quiet "refs/heads/$1"; }

list_candidates() {
  git -C "$SRC_PATH" for-each-ref --format='%(refname:short)' refs/heads | head -20 | tr '\n' ' '
}

# find_checkout: path of the worktree (if any) that has refs/heads/<branch> checked out.
find_checkout() {
  git -C "$SRC_PATH" worktree list --porcelain | awk -v b="refs/heads/$1" '
    /^worktree / { p = substr($0, 10) }
    /^branch /   { if (substr($0, 8) == b) { print p; exit } }'
}

# --- parse args --------------------------------------------------------------

while [ $# -gt 0 ]; do
  case "$1" in
    --squash)  MODE=squash ;;
    --ff)      MODE=ff ;;
    --into)    shift; [ $# -gt 0 ] || die "--into needs a value"; INTO=$1 ;;
    --message) shift; [ $# -gt 0 ] || die "--message needs a value"; MESSAGE=$1 ;;
    --keep)    KEEP=true ;;
    --wip)     WIP=true ;;
    --dry-run) DRY_RUN=true ;;
    -h|--help) usage; exit 0 ;;
    --*)       die "unknown option: $1" ;;
    *)         [ -z "$SELECTOR" ] || die "unexpected extra argument: $1"; SELECTOR=$1 ;;
  esac
  shift
done

[ -n "$SELECTOR" ] || { usage; exit 1; }

# Accept a bare name for convenience: normalize it to Orca's `name:` selector.
case "$SELECTOR" in
  *:*)                 ;;  # already prefixed (id:, path:, name:, branch:, …)
  active|current)      ;;
  *) SELECTOR="name:$SELECTOR" ;;
esac

command -v jq   >/dev/null 2>&1 || die "jq is required"
command -v "$ORCA_BIN" >/dev/null 2>&1 || die "orca executable not found: $ORCA_BIN"

# --- resolve the worktree ----------------------------------------------------

WT_JSON=$(orca_probe worktree show --worktree "$SELECTOR" --json)
WT_ID=$(jq -r '.result.worktree.id // ""'                    <<<"$WT_JSON")
SRC_PATH=$(jq -r '.result.worktree.path // ""'               <<<"$WT_JSON")
IS_MAIN=$(jq -r '.result.worktree.isMainWorktree // false'   <<<"$WT_JSON")
BRANCH_REF=$(jq -r '.result.worktree.git.branch // .result.worktree.branch // ""' <<<"$WT_JSON")
BASE_REF=$(jq -r '.result.worktree.baseRef // ""'            <<<"$WT_JSON")
PARENT_ID=$(jq -r '.result.worktree.parentWorktreeId // ""'  <<<"$WT_JSON")
REPO_ID=$(jq -r '.result.worktree.repoId // ""'             <<<"$WT_JSON")
BRANCH=${BRANCH_REF#refs/heads/}

# Prefer git's live view of the checked-out branch: Orca's cached metadata can lag
# (e.g. it may still report detached immediately after a `git rebase --continue`).
GIT_BRANCH=$(git -C "$SRC_PATH" rev-parse --abbrev-ref HEAD 2>/dev/null || echo "")
if [ -n "$GIT_BRANCH" ] && [ "$GIT_BRANCH" != "HEAD" ]; then
  BRANCH=$GIT_BRANCH
fi

[ -n "$WT_ID" ]    || die "could not resolve worktree '$SELECTOR'"
[ -d "$SRC_PATH" ] || die "worktree path does not exist: $SRC_PATH"
[ "$IS_MAIN" != true ] || die "refusing to integrate the repo's main worktree"
[ -n "$BRANCH" ]   || die "worktree has no branch (detached); nothing to integrate"

# The repo's primary worktree, used for ref/branch operations after the source
# worktree is removed.
MAIN_PATH=$(git -C "$SRC_PATH" worktree list --porcelain | awk '/^worktree /{print substr($0,10); exit}')
[ -n "$MAIN_PATH" ] || MAIN_PATH=$SRC_PATH

# --- determine the target branch ---------------------------------------------
# Precedence: --into → parent worktree's branch → the worktree's recorded
# baseRef → repo trunk (main/master) → the main worktree's branch.

TARGET=""
TARGET_SOURCE=""

if [ -n "$INTO" ]; then
  cand=${INTO#refs/heads/}
  branch_exists "$cand" || die "target branch '$cand' not found. Local branches: $(list_candidates)"
  TARGET=$cand; TARGET_SOURCE="--into"
fi

if [ -z "$TARGET" ] && [ -n "$PARENT_ID" ]; then
  if pj=$(orca_maybe worktree show --worktree "id:$PARENT_ID" --json); then
    pj_repo=$(jq -r '.result.worktree.repoId // ""' <<<"$pj")
    pb=$(jq -r '.result.worktree.git.branch // .result.worktree.branch // ""' <<<"$pj")
    pb=${pb#refs/heads/}
    # Ignore a parent that lives in a different repo (can happen if Orca inferred
    # the parent from the caller's cwd rather than the worktree's own lineage).
    if [ "$pj_repo" = "$REPO_ID" ] && [ -n "$pb" ] && branch_exists "$pb"; then
      TARGET=$pb; TARGET_SOURCE="parent worktree"
    fi
  fi
fi

if [ -z "$TARGET" ] && [ -n "$BASE_REF" ]; then
  cand=${BASE_REF#refs/heads/}; cand=${cand#refs/remotes/}
  if branch_exists "$cand"; then
    TARGET=$cand; TARGET_SOURCE="baseRef"
  elif [ "$cand" != "${cand#origin/}" ] && branch_exists "${cand#origin/}"; then
    TARGET=${cand#origin/}; TARGET_SOURCE="baseRef"
  fi
fi

if [ -z "$TARGET" ]; then
  for c in main master; do
    if branch_exists "$c"; then TARGET=$c; TARGET_SOURCE="trunk"; break; fi
  done
fi

if [ -z "$TARGET" ]; then
  mb=$(git -C "$MAIN_PATH" rev-parse --abbrev-ref HEAD 2>/dev/null || echo "")
  if [ -n "$mb" ] && [ "$mb" != "HEAD" ] && branch_exists "$mb"; then
    TARGET=$mb; TARGET_SOURCE="main worktree"
  fi
fi

[ -n "$TARGET" ] || die "could not determine a target branch; pass --into. Local branches: $(list_candidates)"
[ "$TARGET" != "$BRANCH" ] || die "target branch equals the worktree branch ($BRANCH)"

# --- preflight (abort before touching anything) ------------------------------

if [ -n "$(git -C "$SRC_PATH" status --porcelain)" ]; then
  if $WIP; then
    DIRTY=1
  else
    die "worktree is dirty; commit/stash, or pass --wip to commit it as 'wip: $BRANCH'"
  fi
else
  DIRTY=0
fi

TL_JSON=$(orca_probe terminal list --worktree "$WT_ID" --json)
TL_COUNT=$(jq '[.result.terminals[]? | select(.orphaned != true)] | length' <<<"$TL_JSON")
[ "$TL_COUNT" = 0 ] || die "worktree has $TL_COUNT live terminal(s); close the agent terminals first"

TARGET_CHECKOUT=$(find_checkout "$TARGET")
if [ -n "$TARGET_CHECKOUT" ]; then
  [ -z "$(git -C "$TARGET_CHECKOUT" status --porcelain)" ] \
    || die "target checkout is dirty: $TARGET_CHECKOUT (commit/stash first)"
fi

TARGET_SHA=$(git -C "$SRC_PATH" rev-parse --verify "refs/heads/$TARGET")
SOURCE_SHA=$(git -C "$SRC_PATH" rev-parse --verify "$BRANCH")
IS_FF=0
if git -C "$SRC_PATH" merge-base --is-ancestor "refs/heads/$TARGET" "$BRANCH"; then IS_FF=1; fi

MSG=${MESSAGE:-"$BRANCH: integrate worktree work"}

# --- plan --------------------------------------------------------------------

info "orca-integrate"
info "  worktree : $WT_ID"
info "  path     : $SRC_PATH"
info "  branch   : $BRANCH"
info "  target   : $TARGET  (from $TARGET_SOURCE)"
info "  mode     : $MODE"
info "  message  : $MSG"
if [ -n "$TARGET_CHECKOUT" ]; then
  info "  checkout : $TARGET_CHECKOUT (clean)"
else
  info "  checkout : none (target ref updated directly)"
fi
if $DRY_RUN; then info "  DRY RUN  : no changes will be made"; fi

# --- temp-worktree bookkeeping ----------------------------------------------

TEMP_WT=""
KEEP_TEMP=0
cleanup_temp() {
  [ -n "$TEMP_WT" ] || return 0
  [ -d "$TEMP_WT" ] || return 0
  if [ "$KEEP_TEMP" = 1 ]; then
    warn "temp worktree left at $TEMP_WT (holds an unreferenced commit; inspect before removing)"
  else
    git -C "$MAIN_PATH" worktree remove --force "$TEMP_WT" >/dev/null 2>&1 || true
  fi
}
trap cleanup_temp EXIT

# --- integrate ---------------------------------------------------------------

if [ "$DIRTY" = 1 ]; then
  info "worktree is dirty; committing as 'wip: $BRANCH' (--wip)"
  run git -C "$SRC_PATH" add -A
  run git -C "$SRC_PATH" commit -m "wip: $BRANCH"
  if ! $DRY_RUN; then
    SOURCE_SHA=$(git -C "$SRC_PATH" rev-parse "$BRANCH")
    IS_FF=0
    if git -C "$SRC_PATH" merge-base --is-ancestor "refs/heads/$TARGET" "$BRANCH"; then IS_FF=1; fi
  fi
fi

if [ "$MODE" = ff ] && [ "$IS_FF" = 1 ]; then
  info "fast-forwarding '$TARGET' to '$BRANCH'"
  if [ -n "$TARGET_CHECKOUT" ]; then
    run git -C "$TARGET_CHECKOUT" merge --ff-only "$BRANCH" \
      || die "fast-forward failed in $TARGET_CHECKOUT"
  else
    run git -C "$SRC_PATH" update-ref "refs/heads/$TARGET" "$SOURCE_SHA" "$TARGET_SHA" \
      || die "failed to fast-forward refs/heads/$TARGET"
  fi

elif [ "$MODE" = ff ]; then
  info "'$TARGET' has diverged from '$BRANCH'; rebasing '$BRANCH' onto '$TARGET', then fast-forwarding"
  run git -C "$SRC_PATH" rebase "refs/heads/$TARGET" \
    || die "rebase conflicted. Resolve in $SRC_PATH (git rebase --continue), then re-run this script."
  if ! $DRY_RUN; then
    SOURCE_SHA=$(git -C "$SRC_PATH" rev-parse "$BRANCH")
  fi
  if [ -n "$TARGET_CHECKOUT" ]; then
    run git -C "$TARGET_CHECKOUT" merge --ff-only "$BRANCH" \
      || die "fast-forward failed in $TARGET_CHECKOUT"
  else
    run git -C "$SRC_PATH" update-ref "refs/heads/$TARGET" "$SOURCE_SHA" "$TARGET_SHA" \
      || die "failed to fast-forward refs/heads/$TARGET"
  fi

else
  info "squash-merging '$BRANCH' into '$TARGET'"
  if [ -n "$TARGET_CHECKOUT" ]; then
    run git -C "$TARGET_CHECKOUT" merge --squash "$BRANCH" || {
      if ! $DRY_RUN; then git -C "$TARGET_CHECKOUT" reset --hard >/dev/null 2>&1 || true; fi
      die "squash merge conflicted; resolve manually: git -C '$TARGET_CHECKOUT' merge --squash '$BRANCH'"
    }
    run git -C "$TARGET_CHECKOUT" commit -m "$MSG" || die "commit failed in $TARGET_CHECKOUT"
  else
    TMP=$(mktemp -d); rmdir "$TMP"
    TEMP_WT=$TMP
    run git -C "$SRC_PATH" worktree add --detach "$TMP" "refs/heads/$TARGET" \
      || die "failed to create temp worktree at $TMP"
    run git -C "$TMP" merge --squash "$BRANCH" || die "squash merge conflicted in temp worktree"
    run git -C "$TMP" commit -m "$MSG" || die "commit failed in temp worktree"
    if ! $DRY_RUN; then
      NEW=$(git -C "$TMP" rev-parse HEAD)
      KEEP_TEMP=1
      git -C "$SRC_PATH" update-ref "refs/heads/$TARGET" "$NEW" "$TARGET_SHA" \
        || die "failed to move refs/heads/$TARGET to the squashed commit"
      git -C "$SRC_PATH" worktree remove "$TMP" || die "failed to remove temp worktree $TMP"
      TEMP_WT=""
    else
      run git -C "$SRC_PATH" update-ref "refs/heads/$TARGET" "<new-commit>" "$TARGET_SHA"
      run git -C "$SRC_PATH" worktree remove "$TMP"
      TEMP_WT=""
    fi
  fi
fi

# --- cleanup -----------------------------------------------------------------

if $KEEP; then
  info "keeping worktree and branch (--keep)"
else
  info "removing Orca worktree and deleting branch '$BRANCH'"
  run_orca "$ORCA_BIN" worktree rm --worktree "$WT_ID" --json \
    || die "orca worktree rm failed"
  if ! $DRY_RUN; then
    if git -C "$MAIN_PATH" show-ref --verify --quiet "refs/heads/$BRANCH"; then
      git -C "$MAIN_PATH" branch -D "$BRANCH" || warn "could not delete branch $BRANCH"
    fi
    still=$(orca_probe worktree list --json | jq -r --arg id "$WT_ID" '[.result.worktrees[]? | select(.id==$id)] | length')
    [ "$still" = 0 ] || warn "worktree still registered with Orca: $WT_ID"
  else
    run git -C "$MAIN_PATH" branch -D "$BRANCH"
  fi
fi

info "done."
