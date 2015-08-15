
source ~/.config/bash/git-prompt.sh

set_prompt() {
    lastcmd=$?
    blue="$(tput setaf 33)"
    green="$(tput setaf 41)"
    white="$(tput setaf 15)"
    red="$(tput setaf 196)"
    gray="$(tput setaf 240)"
    reset="$(tput sgr0)"
    arrow=" →  "

    if [[ lastcmd -eq "0" ]]; then
        namecolor="$green"
    else
        namecolor="$red"
    fi

    GIT_PS1_SHOWSTASHSTATE=1
    GIT_PS1_SHOWDIRTYSTATE=1
    GIT_PS1_SHOWUNTRACKEDFILES=1
    GIT_PS1_SHOWCOLORHINTS=1
    GIT_PS1_DESCRIBE_STYLE="branch"
    GIT_PS1_SHOWUPSTREAM="git verbose auto"
    GIT_PS1_HIDE_IF_PWD_IGNORED=1

    PS1='\n'
    PS1+='\[$namecolor\]\u'
    PS1+='\[$white\]@\[$blue\]\h'
    PS1+=' \[$white\]in \w'
    PS1+='\n'
    PS1+='\[$gitcolor\]$(__git_ps1 " (%s)")'
    PS1+='\[$white\]$arrow'
    PS1+='\[$reset\]'
}

PROMPT_COMMAND='set_prompt'
