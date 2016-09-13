
# sets PS1
source ~/.config/bash/prompt.sh

# export variables
source ~/.config/bash/exports.sh

# creates aliases
source ~/.config/bash/aliases.sh

# creates functions
source ~/.config/bash/functions.sh

# enables bash completion
source ~/.config/bash/completion.sh

# set vim-style keybindings
set -o vi

# append to history on exit instead of overwriting
shopt -s histappend

# save multi-line commands as one history entry
shopt -s cmdhist

# import dircolors
eval $(dircolors ~/.config/bash/dircolors)
