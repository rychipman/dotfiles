# Global Instructions

## Planning

- In plan mode, always write the plan to a markdown file under
  `~/.opencode/plan/<topic>.md` instead of printing it in chat.
- After writing it, open the file for preview in the Review pane (desktop
  app) so I can read and comment there; in the TUI, just tell me the path.
- Treat the plan file as the source of truth: when I give feedback, revise the
  plan in a file rather than restating it in chat.
- Never overwrite an existing plan file: the Review pane caches file contents
  and will not show in-place edits. Each revision must be a new file with an
  incrementing version suffix, and you must open the newest one for preview:
  - `<topic>.md` (first version)
  - `<topic>.v2.md` (first revision)
  - `<topic>.v3.md` (second revision)
  - ... and so on, `<topic>.vN.md` for the Nth revision.
- Before switching out of plan mode, confirm the newest plan file is current
  and note its path.
