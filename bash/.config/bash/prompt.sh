
source ~/.config/bash/git-prompt.sh

git_status() {
    GIT_PS1_SHOWSTASHSTATE=1
    GIT_PS1_SHOWDIRTYSTATE=1
    GIT_PS1_SHOWUNTRACKEDFILES=1
    GIT_PS1_SHOWCOLORHINTS=true
    GIT_PS1_DESCRIBE_STYLE="branch"
    GIT_PS1_SHOWUPSTREAM="git verbose auto"
    GIT_PS1_HIDE_IF_PWD_IGNORED=1
    echo "$(__git_ps1 " (%s)")"
}

stoppedjobs() {
    numjobs=$(jobs -s | wc -l | sed -e "s/ //g")
    if [ $numjobs -ne 0 ]; then
        echo " ($numjobs)"
    fi
}

set_prompt() {
    lastcmd=$?
    blue="$(tput setaf 33)"
    green="$(tput setaf 41)"
    white="$(tput setaf 15)"
    red="$(tput setaf 196)"
    gray="$(tput setaf 240)"
    orange="$(tput setaf 202)"
    reset="$(tput sgr0)"
    arrow=" →  "

    if [[ lastcmd -ne "0" ]]; then
        namecolor="$red"
    elif [[ $EUID -ne 0 ]]; then
        namecolor="$green"
    else
	namecolor="$orange"
    fi


    PS1='\n'
    PS1+='\[$namecolor\]\u'
    PS1+='\[$white\]@\[$blue\]\h'
    PS1+=' \[$white\]in \w'
    PS1+='\n'
    PS1+='\[$gray\]$(git_status)'
    PS1+='$(stoppedjobs)'
    PS1+='\[$white\]$arrow'
    PS1+='\[$reset\]'
}

PROMPT_COMMAND='set_prompt'

export PS2=" ⟼  "
