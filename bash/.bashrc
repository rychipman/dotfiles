
# sets PS1
source ~/.config/bash/prompt.sh

# export variables
source ~/.config/bash/exports.sh

# creates aliases
source ~/.config/bash/aliases.sh

# enables bash completion
source ~/.config/bash/completion.sh

# set vim-style keybindings
set -o vi

# import dircolors
eval $(dircolors ~/.config/bash/dircolors)

