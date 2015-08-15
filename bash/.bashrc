
# sets PS1
source ~/.config/bash/prompt.sh

# add personal scripts and binaries to path
export PATH=~/bin:$PATH
export PATH=~/scripts:$PATH

export GOPATH=~/development/go
export GOBIN=$GOPATH/bin
export PATH=$GOBIN:$PATH

export STOW_DIR=~/.dotfiles

# Set colors for ls
export CLICOLOR=1
export LSCOLORS=Gxfxcxdxbxegedabagacad

# Settings for some default executables
export LESS='--ignore-case --raw-control-chars'
export PAGER='less'
export EDITOR='vim'

# set vim-style keybindings
set -o vi

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

# import dircolors
eval $(dircolors ~/.config/bash/dircolors)

export TERM=screen-256color
export TMPDIR=/tmp

# tmux convenience aliases
alias tt='tmux attach-session -t'
