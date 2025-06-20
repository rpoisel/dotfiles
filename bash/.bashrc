# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

#shellspec shell=bash

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
    xterm*|rxvt*)
        PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
        ;;
    *)
        ;;
esac

# locale settings
export LC_MEASUREMENT="de_DE.UTF-8"
export LANGUAGE="en_US.UTF-8"

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    #alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    #alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# source /usr/share/bash-completion/completions/git
# source /usr/share/bash-completion/completions/docker-compose
if [ -d "${HOME}/.bash_completion.d" ]; then
  for i in $(find "${HOME}/.bash_completion.d/" -type f | sort); do
    source "${i}"
  done
fi

# some more ls aliases
# alias ll='ls -l'
# alias la='ls -lAh'
# alias l='ls -CF'
alias ll='exa -l'
alias la='exa -la'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f "${HOME}/.bash_aliases" ]; then
    . "${HOME}/.bash_aliases"
fi

if [ -f "${HOME}/.rpo_functions" ]; then
    . "${HOME}/.rpo_functions"
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# General settings
export PATH="${HOME}/.local/bin:${PATH}:${HOME}/go/bin"
export EDITOR="emacs -nw"

shopt -s globstar

# Terminal configuration
if ! [ "${TERM}" == "dumb" ]; then
  source /usr/share/doc/fzf/examples/key-bindings.bash
  # Using highlight (http://www.andre-simon.de/doku/highlight/en/highlight.html)
  export FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2> /dev/null | head -200'"
  source "${HOME}/.dotfiles/fzf-marks/fzf-marks.plugin.bash"
fi

[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/bash"

eval "$(direnv hook bash)"
if ! [ "${TERM}" == "dumb" ]; then
  eval "$(starship init bash)"
fi

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTCONTROL=ignorespace:erasedups               # leading space hides commands from history
HISTFILESIZE=10000                              # increase history file size (default is 500)
HISTSIZE=${HISTFILESIZE}                        # increase history size (default is 500)
HISTIGNORE="la:ll:ls:cd:pwd:exit"               # will never make it into shell history
shopt -s histappend                             # append on shell exit
PROMPT_COMMAND="history -a; history -n; $PROMPT_COMMAND"

[ -s "${HOME}/.cargo/env" ] && source "${HOME}/.cargo/env"

# Google Cloud SDK
if [ -e "${HOME}/google-cloud-sdk/path.bash.inc" ]; then
    source "${HOME}/google-cloud-sdk/path.bash.inc"
    source "${HOME}/google-cloud-sdk/completion.bash.inc"
fi

# Gradle
export PATH="${PATH}:${HOME}/.local/opt/gradle/bin"

# Shell completion
function add_completion() {
  cmd="${1}"
  shift

  if ! command -v "${cmd}" > /dev/null; then
    return
  fi

  if (( "$#" )); then
    source <("${cmd}" "${@}")
  else
    source <("${cmd}" completion bash)
  fi
}

add_completion kubectl
add_completion kompose
add_completion goreleaser
add_completion k3d
add_completion hugo
add_completion yq shell-completion bash
add_completion gh completion -s bash
add_completion podman

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

. "$HOME/.cargo/env"
