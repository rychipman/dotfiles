
# add gnu coreutils to path
export PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH

# add gnu coreutils manpages to manpath
export MANPATH=/usr/local/opt/coreutils/libexec/gnuman:$MANPATH

# Set colors for ls
export CLICOLOR=1
export LSCOLORS=Gxfxcxdxbxegedabagacad

# Settings for some default executables
export LESS='--ignore-case --raw-control-chars'
export PAGER='less'
export EDITOR='vim'

# 256-color terminal
export TERM=screen-256color

# set tmpdir
export TMPDIR=/tmp

export PKG_CONFIG_PATH="/usr/local/Cellar/openssl/1.0.2n/lib/pkgconfig:$PKG_CONFIG_PATH"
