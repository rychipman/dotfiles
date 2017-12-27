# append to history on exit instead of overwriting
shopt -s histappend

# save multi-line commands as one history entry
shopt -s cmdhist

# ignore duplicate commands and commands starting with a space
export HISTCONTROL=ignorespace:ignoredups

# histfile settings
export HISTFILE=~/.local/share/bash_eternal_history
export HISTSIZE=
export HISTFILESIZE=
export HISTTIMEFORMAT="[%F %T] "

# save every line to history (protects against weird bash exits)
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"
