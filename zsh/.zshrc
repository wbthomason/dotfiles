# Load plugins
export ZSH_PLUGINS=/usr/share/zsh/plugins/

export ZSH_AUTOSUGGEST_USE_ASYNC=1
# source ${ZSH_PLUGINS}/zsh-autosuggestions/zsh-autosuggestions.zsh

source ${ZSH_PLUGINS}/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh

source ${ZSH_PLUGINS}/zsh-history-substring-search/zsh-history-substring-search.zsh
# source ~/zsh-prompt-benchmark.plugin.zsh
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# History
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory
# Don't want common history between shells
unsetopt share_history

# pkg-config tweaks
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig

# LD_LIBRARY_PATH tweaks
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib:/usr/local/lib64

# PATH tweaks
export PATH=$PATH:/home/wil/.local/bin:/home/wil/.cargo/bin:/home/wil/.luarocks/bin:/home/wil/.roswell/bin

# Editor setting
export EDITOR=vim

# ROS
source /opt/ros/melodic/setup.zsh
export ROS_HOSTNAME=localhost
export ROS_MASTER_URI=http://localhost:11311
export ROS_PYTHON_VERSION=3

# Hub
eval "$(hub alias -s)"

# Completion
fpath=(~/.zsh/completions $fpath) 
autoload -Uz compinit
if [[ -n ~/.zcompdump(#qN.mh+24) ]]; then
  compinit
else
  compinit -C
fi

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
alias ls='ls --color=auto'

function open() {
  xdg-open $1 > /dev/null 2>&1 &
}

# Prompt
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

# Auto-cd
setopt auto_cd

# Vi mode
bindkey -v

# FZF
export FZF_DEFAULT_COMMAND='rg --files --hidden --smart-case --glob "!.git/*"'
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh
