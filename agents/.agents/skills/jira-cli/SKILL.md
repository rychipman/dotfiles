---
name: Jira CLI
description: Use the local `jira` CLI to search, view, create, and update MongoDB Jira issues (server jira.mongodb.org) with bearer auth from 1Password.
---

# Jira CLI

The machine has `ankitpokhrel/jira-cli` configured for MongoDB Jira.

## Setup facts

- Server: `https://jira.mongodb.org`, login `ryan.chipman@mongodb.com`
- Config file: `~/.config/.jira/.config.yml`
- Auth type is `bearer`. A shell wrapper named `jira` fetches the API token
  from 1Password on demand, so **just run `jira …` — do not export
  `JIRA_API_TOKEN` yourself**.
- The wrapper is defined in `~/.config/bash/contrib/jira.sh` (dotfiles `jira`
  package). `jira-token` prints the raw token if another tool needs it:
  `export JIRA_API_TOKEN="$(jira-token)"` (e.g. in a `direnv` `.envrc`).
- If `jira` is "not found", the shell wrapper was not sourced — run it from a
  login shell rather than calling the binary directly.

## Common commands

```sh
jira issue list -a"$(jira me)"            # my open issues
jira issue list -q 'project = BF AND status = "Needs Triage"'
jira issue view BF-12345                  # details + comments
jira issue view BF-12345 --comments 5     # with last 5 comments
jira issue create -tBug -s"Summary" -b"Description"
jira issue comment add BF-12345 "text"
jira issue move BF-12345 "In Progress"    # transition status
jira me                                    # who am I
```

## Notes

- `-q` takes raw JQL; quote it. Prefer server-side filters over piping to
  `grep`.
- For MongoDB build-failure work, the BF project and its "Assigned Teams"
  field are the usual entry points.
- `jira issue list` supports `--plain`, `--no-headers`, and `--raw` for
  scripting; use `--raw` when parsing output in a script.
