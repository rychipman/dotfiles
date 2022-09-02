
# directory aliases
alias ls='ls --color'
alias sl='ls'
alias la='ls -a'
alias ll='ls -lh'
alias lla='ls -lah'
alias mkdir='mkdir -pv'
alias cd='cd -P'

# grep, searching stuff
alias grep='grep --color'

# terminal history manipulation
alias c='clear'
alias h='history'
alias j='jobs -l'

# network aliases
alias ping='ping -c 4'

# add some safety confirmations
alias mv='mv -i'
alias cp='cp -i'
alias ln='ln -i'

# allow completion in sudo commands
alias sudo='sudo '

# tmux convenience aliases
alias tt='tmux attach-session -t'

# dropbox
alias dbx='dropbox-cli'

# get better view of permissions for file
alias perm='stat -c "%A %a %n"'
