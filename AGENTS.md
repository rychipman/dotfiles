# AGENTS.md

`CLAUDE.md` imports this file via `@AGENTS.md`; keep this the canonical instruction file.

Personal dotfiles managed with **GNU Stow**. macOS (Darwin) only. There is no build, lint, or test suite — the repo is configuration files.

## Layout

- The repo lives at `~/dotfiles`. Each top-level directory (except `.git` and `.claude`) is a **stow package** whose contents mirror `$HOME`.
- Examples: `vim/.vim/` -> `~/.vim/`, `git/.gitconfig` -> `~/.gitconfig`, `bin/bin/*` -> `~/bin/*`.
- Editing a file here only affects `$HOME` after it is stowed. Stow symlinks individual files, so edits to already-stowed files are live; **newly added files need a re-stow** to appear.

## Apply changes

Run from `~/dotfiles`:

```bash
stow <package>        # create symlinks for a package
stow -R <package>     # re-stow after adding/removing files
stow -D <package>     # remove a package's symlinks
stow -n -v <package>  # dry run to verify before applying
```

`stow` resolves packages relative to the cwd unless `STOW_DIR` is set. The `stow` package exports `STOW_DIR=~/dotfiles`, but only in login shells, so run from `~/dotfiles` (or set `STOW_DIR=~/dotfiles`) when an agent shell may not have it.

## Shell config

- `~/.bashrc` sources `bash/.config/bash/bashrc`, which loads the core modules in `bash/.config/bash/` and then every file in `~/.config/bash/contrib/*`.
- To add shell config for a package, create `<package>/.config/bash/contrib/<package>.sh`; it is sourced automatically. Existing examples: `jira`, `llm`, `bin`, `doom`, `ledger`, `direnv`, `stow`.
- `bin/.config/bash/contrib/bin.sh` prepends `~/bin` to `PATH`.

## Packages

| Package | Purpose |
|---------|---------|
| agents | Authored global agent skills, stowed to `~/.agents/skills` |
| bash | Shell config, prompt, aliases, functions |
| bin | Personal scripts, stowed to `~/bin` |
| claude | `~/.claude/settings.json` |
| direnv | Per-directory environment management |
| doom | Doom Emacs config |
| emacs | Vanilla Emacs config |
| git | Git config, aliases, hooks, global ignore |
| go | Go environment variables |
| hammerspoon | macOS automation (Lua) |
| homebrew | Homebrew shell integration |
| jira | Jira CLI with 1Password token integration |
| karabiner | macOS keyboard remapping |
| ledger | Ledger accounting tool |
| llm | LLM CLI config, templates, model definitions |
| mongodb | MongoDB shell and tools |
| nvim-old | Legacy Neovim config (nvim replaced by `vim`) |
| opencode | OpenCode config (MCP servers, etc.) |
| rust | Cargo/Rust environment |
| ssh | SSH config |
| stow | Stow's own config (`STOW_DIR`, global ignore) |
| tmux | Terminal multiplexer |
| vim | Vim config with vim-plug |

## Global agent skills

Authored skills live in `agents/.agents/skills/<id>/SKILL.md` and stow to
`~/.agents/skills`, the canonical cross-agent location. OpenCode reads that
directory natively; `~/.claude/skills/<id>` is a per-skill symlink into it for
Claude Code. Third-party skills installed there by the `vercel-labs/skills` CLI
(and its `~/.agents/.skill-lock.json`) are not vendored into this repo.

Adding a skill needs a re-stow plus a Claude symlink:

```bash
cd ~/dotfiles && stow -R agents
for d in agents/.agents/skills/*/; do
  ln -sfn "../../.agents/skills/$(basename "$d")" ~/.claude/skills/"$(basename "$d")"
done
```

## Git conventions

- Global config is `git/.gitconfig`; it rewrites `https://github.com` to SSH and sets `core.excludesfile = ~/.config/git/ignore`.
- `git/.config/git/hooks/` holds `bin/` and `runners/` hook scripts, but **no `core.hooksPath` is set**, so they are not globally wired up.
- Commit messages are prefixed with the package name, lowercase: `git: Add .DS_Store to global gitignore`, `hammerspoon: Refactor to declarative config`. Older commits may omit the prefix.

## Conventions

- Most packages contain an empty `.stow-local-ignore` as a marker. It is not required — `doom`, `emacs`, `ledger`, `nvim-old`, `rust`, and `stow` omit it.
- `.gitignore` excludes plugin downloads, Emacs `elpa`/`var`/`etc`, logs, and other generated files.
- Several aliases and scripts are macOS-specific; avoid Linux-only assumptions.

## Adding a new package

1. `mkdir -p <package>/.config/<app>` and mirror the home structure.
2. (Optional) Add an empty `.stow-local-ignore`.
3. (Optional) Add shell integration at `<package>/.config/bash/contrib/<package>.sh`.
4. Run `stow <package>` from `~/dotfiles`.
