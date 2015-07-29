username="$(if [[ \$? != "0" ]]; then echo "\\[\\033[38;5;41m\\]"; else echo "\\[\\033[31m\\]"; fi)\u\[$(tput sgr0)\]"
export PS1="\n$username\[\033[38;5;15m\]@\[$(tput sgr0)\]\[\033[38;5;33m\]\h\[$(tput sgr0)\]\[\033[38;5;15m\] in \w\[$(tput sgr0)\]\[\033[38;5;83m\]\n\[$(tput sgr0)\]\[\033[38;5;15m\] > \[$(tput sgr0)\]"

# add personal scripts and binaries to path
export PATH=~/bin:$PATH
export PATH=~/scripts:$PATH

export GOPATH=~/development/go
export GOBIN=$GOPATH/bin
export PATH=$GOBIN:$PATH

# Set colors for ls
export CLICOLOR=1
export LSCOLORS=Gxfxcxdxbxegedabagacad

# Settings for some default executables
export LESS='--ignore-case --raw-control-chars'
export PAGER='less'
export EDITOR='vim'

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

export TERM=screen-256color
export TMPDIR=/tmp
