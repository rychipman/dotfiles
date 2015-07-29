username="$(if [[ \$? != "0" ]]; then echo "\\[\\033[38;5;41m\\]"; else echo "\\[\\033[31m\\]"; fi)\u\[$(tput sgr0)\]"
export PS1="\n$username\[\033[38;5;15m\]@\[$(tput sgr0)\]\[\033[38;5;33m\]\h\[$(tput sgr0)\]\[\033[38;5;15m\] in \w\[$(tput sgr0)\]\[\033[38;5;83m\]\n\[$(tput sgr0)\]\[\033[38;5;15m\] > \[$(tput sgr0)\]"

# add personal scripts and binaries to path
export PATH=~/bin:$PATH
export PATH=~/scripts:$PATH
