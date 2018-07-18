# Load plugins
export ZSH="$(antibody home)/https-COLON--SLASH--SLASH-github.com-SLASH-robbyrussell-SLASH-oh-my-zsh"
source ~/.zsh/load_plugins.zsh

# Find-the-command
source /usr/share/doc/find-the-command/ftc.zsh

# pkg-config tweaks
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig

# LD_LIBRARY_PATH tweaks
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib:/usr/local/lib64

# PATH tweaks
export PATH=$PATH:/home/wil/.local/bin:/home/wil/.cargo/bin:/home/wil/.luarocks/bin:/home/wil/.roswell/bin

# Editor setting
export EDITOR=vim

# Rust
export RUST_SRC_PATH=/usr/src/rust/src

# ROS
source /opt/ros/melodic/setup.zsh
export ROS_HOSTNAME=localhost
export ROS_MASTER_URI=http://localhost:11311

# FZF
export FZF_DEFAULT_COMMAND='rg --files --hidden --smart-case --glob "!.git/*"'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Hub
eval "$(hub alias -s)"
fpath=(~/.zsh/completions $fpath) 
autoload -U compinit && compinit

# Thefuck
eval $(thefuck --alias)

# gnome-keyring
if [ -n "$DESKTOP_SESSION" ];then
    eval $(gnome-keyring-daemon --start --components=gpg,ssh,secrets)
    export SSH_AUTH_SOCK
fi

# Aliases
alias svn='colorsvn'
alias eclimd='/usr/lib/eclipse/eclimd -b'
alias em='emacsclient -nw'
alias emd='emacs --daemon'
alias git='hub'

function zle-line-init zle-keymap-select {
  PROMPT=`~/projects/personal/purs/target/release/purs prompt -k "$KEYMAP" -r "$?"`
  zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

autoload -Uz add-zsh-hook

function _prompt_purs_precmd() {
  ~/projects/personal/purs/target/release/purs precmd
}
add-zsh-hook precmd _prompt_purs_precmd
