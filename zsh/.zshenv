if [[ -n $VIRTUAL_ENV && -e "${VIRTUAL_ENV}/bin/activate" ]]; then
  source "${VIRTUAL_ENV}/bin/activate"
fi

# PATH tweaks
export PATH=$PATH:/home/wil/.gem/ruby/2.7.0/bin:/home/wil/.yarn/bin:/home/wil/.local/bin:/home/wil/.cargo/bin:/home/wil/.luarocks/bin:/home/wil/.roswell/bin

# gnome-keyring
if [ -n "$DESKTOP_SESSION" ];then
    eval $(gnome-keyring-daemon --start --components=gpg,ssh,secrets 2>/dev/null)
    export SSH_AUTH_SOCK
fi

BROWSER=firefox

export GRB_LICENSE_FILE=/home/wil/.gurobi/gurobi.lic
