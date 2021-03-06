#
# ~/.bashrc
#

# Custom variables


[[ $- != *i* ]] && return # if not running interactively, don't do anything

for FILE in `find ~/.bash_completion -type f -name \*.bash`; do
    source ${FILE}
done

# History options

export HISTFILESIZE=
export HISTSIZE=
export HISTCONTROL="ignoredups"
export HISTIGNORE="[   ]*:&:bg:fg:exit"

# Aliases

alias less='less -R'
alias df='df -h'
alias du='du -h'
alias ls='ls --color=auto'
alias l='ls -l'

alias skype='xhost +local: && sudo -u skype /usr/bin/skype'
alias open='xdg-open'

# Custom terminal line settings

stty stop undef
stty susp ^]

# PS1

PS1='\n\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\[\e[1;35m\]$(__git_ps1 "(%s)")\[\e[m\]\n\$ '

# RVM

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# Editor

export EDITOR=emacs

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
PATH=$PATH:/opt/gradle/bin # Add RVM to PATH for scripting
PATH=$HOME/bin:$PATH

# Set locale

export LANG=en_US.UTF-8
export LC_CTYPE="en_US.UTF-8"

printf '\33]701;%s\007' "$LC_CTYPE"

# Go to last saved directory

function c() {
    cd "$@"
    pwd > ~/.lastdir
}

function c..() {
    c ..
}

[ -s ~/.lastdir ] && cd `cat ~/.lastdir`
