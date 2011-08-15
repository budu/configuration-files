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

PATH=$PATH:~/bin:~/.lein/bin

PS1='\[\e]0;\w\a\]\n\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\[\e[1;35m\]$(__git_ps1 "(%s)")\[\e[m\]\n\$ '

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
