
# directory aliases
alias ls='ls --color'
alias sl='ls'
alias la='ls -a'
alias ll='ls -lh'
alias lla='ls -lah'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias mkdir='mkdir -pv'

# grep, searching stuff
alias grep='grep --color'

# terminal history manipulation
alias c='clear'
alias h='history'
alias j='jobs -l'

# network aliases
alias ping='ping -c 4'

# add some safety confirmations
alias rm='rm -I --preserve-root'
alias mv='mv -i'
alias cp='cp -i'
alias ln='ln -i'
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'

# allow completion in sudo commands
alias sudo='sudo '

# tmux convenience aliases
alias tt='tmux attach-session -t'

# dropbox
alias dbx='dropbox-cli'

# get better view of permissions for file
alias perm='stat -c "%A %a %n"'
