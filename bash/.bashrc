
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

# import dircolors
eval $(dircolors ~/.config/bash/dircolors)

# add gpg key to keyring
eval $(keychain --eval --quiet --agents gpg 2EE67F58)
