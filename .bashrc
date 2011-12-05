#
# ~/.bashrc
#

# Custom variables

export DEV_EMAIL="nbuduroi@gmail.com"

[[ $- != *i* ]] && return # if not running interactively, don't do anything

for FILE in `find ~/.bash_completion -type f -name \*.bash`; do
    source ${FILE}
done

# History options

export HISTCONTROL="ignoredups"
export HISTIGNORE="[   ]*:&:bg:fg:exit"

# Aliases

alias df='df -h'
alias du='du -h'
alias ls='ls --color=auto'
alias l='ls -l'

alias skype='xhost +local: && sudo -u skype /usr/bin/skype'

# Path

export M2_HOME=/opt/maven/
export M2=$M2_HOME/bin
export PATH=$PATH:~/bin:~/.lein/bin:$M2

# Custom terminal line settings

stty stop undef
stty susp ^]

# PS1

PS1='\n\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\[\e[1;35m\]$(__git_ps1 "(%s)")\[\e[m\]\n\$ '

# RVM

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# Editor

export EDITOR=emacs

# ClojureScript

export CLOJURESCRIPT_HOME=$HOME/source/clojurescript
