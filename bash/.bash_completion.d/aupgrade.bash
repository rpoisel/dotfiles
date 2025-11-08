#!/bin/bash

_aupgrade_complete() {
  local cur
  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"

  # nur das erste Argument vervollständigen
  if [[ $COMP_CWORD -eq 1 ]]; then
    COMPREPLY=( $(compgen -W "plan apply" -- "$cur") )
  else
    COMPREPLY=()
  fi
}
complete -F _aupgrade_complete aupgrade
