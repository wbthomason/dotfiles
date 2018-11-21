#!/bin/sh

eval "$(gnome-keyring-daemon --start --components=gpg,ssh,secrets)"
export SSH_AUTH_SOCK

gonvim
