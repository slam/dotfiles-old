if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

if [ -f /etc/bash.bashrc ]; then
	. /etc/bash.bashrc
fi

if [ -f /etc/bash_completion ]; then
	. /etc/bash_completion
fi

if [ -f ~/.bash/aliases ]; then
	source ~/.bash/aliases
fi

if [ -f ~/.bash/completions ]; then
	source ~/.bash/completions
fi

if [ -f ~/.bash/z/z.sh ]; then
	. ~/.bash/z/z.sh
fi

PAGER=less
export PAGER

EDITOR=vim
export EDITOR

set -o vi
shopt -s checkwinsize
export HISTCONTROL=ignoreboth
# append to the history file, don't overwrite it
shopt -s histappend

# put things back on exit
#trap "echo -n \"${XTON}xterm${XTOFF}\"" 0
# save PS1, in case member wants it later
export PLAINPS1="${PS1}"

function prom1
{
case $TERM in
    xterm*|rxvt)
        local TITLEBAR='\[\033]0;\h:\W\007\]'
        local SCREENSCAN=''
        local SCREENTITLE=''
        ;;
    screen*)
        local TITLEBAR='\[\033]0;\h:\W\007\]'
	local SCREENSCAN='\[\033k\033\\\]'
	local SCREENTITLE='\[\033k\W\033\\\]'
        ;;
    *)
        local TITLEBAR=''
	local SCREENSCAN=''
	local SCREENTITLE=''
        ;;
esac

PROMPT_COMMAND='if [ ${#PWD} -gt 30 ]; then myPWD=...${PWD:${#PWD}-27}; else myPWD=$PWD; fi'

command -v git > /dev/null
if [ $? -eq 0 ]
then
  # unstaged: *
  # staged: +
  GIT_PS1_SHOWDIRTYSTATE=
  # stashed: $
  GIT_PS1_SHOWSTASHSTATE=
  # untracked: %
  GIT_PS1_SHOWUNTRACKEDFILES=

  PS1="${TITLEBAR}${SCREENSCAN}${SCREENTITLE}"\
'[\h:$myPWD$(__git_ps1 " (%s)")]\
\$ '
  PS2='> '
  PS4='+ '
else
  PS1="${TITLEBAR}${SCREENSCAN}${SCREENTITLE}"\
'[\h:\W]\
\$ '
  PS2='> '
  PS4='+ '
fi
}

prom1

bashrc_arch=~/.bash/bashrc.`uname`
if [ -f $bashrc_arch ] ; then
	. $bashrc_arch
fi

if [ -f ~/.bash/bashrc.local ] ; then
	. ~/.bash/bashrc.local
fi

if [ -f ~/.bash/aliases.local ]; then
	source ~/.bash/aliases.local
fi

