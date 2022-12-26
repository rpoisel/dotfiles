#!/usr/bin/env bash

function run() {
  if ! pgrep -f "$1"; then
    "$@"&
  fi
}

setxkbmap -layout "us,de" -option "grp:caps_toggle"

run nm-applet
run padevchooser
run blueman-applet
