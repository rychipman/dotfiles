# Dotfiles Repository

Personal configuration files managed with GNU Stow.

## Structure

Each top-level directory is a **stow package** that mirrors the home directory structure:

```
<package>/
├── .config/<app>/        # XDG config dir (~/.config/<app>)
├── .local/share/<app>/   # XDG data dir
├── .<app>/               # Traditional dotfiles (e.g., .vim, .ssh)
└── .stow-local-ignore    # Files excluded from stowing
```

Examples:
- `bash/.config/bash/` → `~/.config/bash/`
- `vim/.vim/` → `~/.vim/`
- `git/.gitconfig` → `~/.gitconfig`

## Shell Configuration Pattern

Bash config uses a **contrib** pattern for modularity:

1. Main bashrc at `bash/.config/bash/bashrc` sources files from `~/.config/bash/contrib/`
2. Each stow package can drop a shell snippet into `.config/bash/contrib/<package>.sh`
3. These are sourced automatically on shell startup

When adding shell-related config for a package, create `<package>/.config/bash/contrib/<package>.sh`.

## Common Operations

**Install a package:**
```bash
cd ~/dotfiles && stow <package>
```

**Remove a package:**
```bash
cd ~/dotfiles && stow -D <package>
```

**Re-stow after changes:**
```bash
cd ~/dotfiles && stow -R <package>
```

**Install all packages:**
```bash
cd ~/dotfiles && stow */
```

## Packages

| Package | Purpose |
|---------|---------|
| bash | Shell config, prompt, aliases, functions |
| bin | Custom scripts (~/.local/bin equivalent at ~/bin) |
| direnv | Per-directory environment management |
| doom | Doom Emacs config |
| emacs | Vanilla Emacs config |
| git | Git config, aliases, hooks, global ignore |
| go | Go environment variables |
| hammerspoon | macOS automation (Lua) |
| homebrew | Homebrew shell integration |
| karabiner | macOS keyboard remapping |
| ledger | Ledger accounting tool |
| llm | LLM CLI tool config and templates |
| mongodb | MongoDB shell and tools |
| nvim-old | Legacy Neovim config |
| rust | Cargo/Rust environment |
| ssh | SSH config |
| stow | Stow's own config |
| tmux | Terminal multiplexer |
| vim | Vim config with vim-plug |

## Adding a New Package

1. Create directory: `mkdir -p <package>/.config/<app>`
2. Add config files mirroring home structure
3. (Optional) Add shell integration: `<package>/.config/bash/contrib/<package>.sh`
4. (Optional) Add `.stow-local-ignore` for files to exclude
5. Run `stow <package>`

## Notes

- Platform: macOS (Darwin)
- The `.gitignore` excludes plugin downloads, caches, and generated files
- Git config uses SSH URL rewriting for GitHub
