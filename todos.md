# Todos / Ideas

Loose ideas and follow-ups. Not a roadmap.

- [ ] **Source OpenCode credentials from 1Password.** Provider keys currently live in the OpenCode server's SQLite database (`~/.local/share/opencode/opencode.db`, `credential` table) and are pasted in by hand. Explore sourcing them from 1Password instead (e.g. `op`-injected env vars, a provider `env` reference, or a plugin) so they aren't hand-managed and can be rotated in one place.
