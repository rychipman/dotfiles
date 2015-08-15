
source ~/.config/bash/git-prompt.sh

set_prompt() {
    lastcmd=$?
    blue='\[\033[38;5;33m\]'
    green='\[\033[38;5;41m\]'
    white='\[\033[38;5;15m\]'
    red='\[\033[38;5;31m\]'
    arrow=" →  "

    if [[ lastcmd -eq "0" ]]; then
        namecolor="$green"
    else
        namecolor="$red"
    fi

    #if git status | grep "nothing to commit" > /dev/null 2>&1; then
    #    gitcolor="$green"
    #else
    #    gitcolor="$red"
    #fi

    GIT_PS1_SHOWSTASHSTATE=1
    GIT_PS1_SHOWDIRTYSTATE=1
    GIT_PS1_SHOWUNTRACKEDFILES=1
    GIT_PS1_SHOWCOLORHINTS=1
    GIT_PS1_DESCRIBE_STYLE="branch"
    GIT_PS1_SHOWUPSTREAM="git verbose auto"
    GIT_PS1_HIDE_IF_PWD_IGNORED=1

    PS1="\n"
    PS1+="${namecolor}\\u"
    PS1+="${white}@${blue}\\h"
    PS1+=" ${white}in \\w"
    PS1+="\n"
    PS1+="${gitcolor}$(__git_ps1 " (%s)")"
    PS1+="${white}$arrow"
}

PROMPT_COMMAND='set_prompt'
