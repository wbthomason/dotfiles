# Load plugins
export ZSH_PLUGINS=/usr/share/zsh/plugins/

export ZSH_AUTOSUGGEST_USE_ASYNC=1
# source ${ZSH_PLUGINS}/zsh-autosuggestions/zsh-autosuggestions.zsh

source ${ZSH_PLUGINS}/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh

source ${ZSH_PLUGINS}/zsh-history-substring-search/zsh-history-substring-search.zsh
# source ~/zsh-prompt-benchmark.plugin.zsh
# bind UP and DOWN arrow keys to history substring search
zmodload zsh/terminfo
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down
bindkey '^[[A' history-substring-search-up			
bindkey '^[[B' history-substring-search-down

# General options
setopt correct
setopt extendedglob
setopt nocaseglob
setopt rcexpandparam
setopt nocheckjobs
setopt numericglobsort

# History
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory
setopt histignorealldups
# Don't want common history between shells
unsetopt share_history

# pkg-config tweaks
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig

# LD_LIBRARY_PATH tweaks
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib:/usr/local/lib64

# Editor setting
export EDITOR=vim

# ROS
source /opt/ros/melodic/setup.zsh
export ROS_HOSTNAME=localhost
export ROS_MASTER_URI=http://localhost:11311
export ROS_PYTHON_VERSION=3

# Hub
eval "$(hub alias -s)"

# Color man pages
export LESS_TERMCAP_mb=$'\E[01;32m'
export LESS_TERMCAP_md=$'\E[01;32m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;47;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;36m'
export LESS=-r

# Completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'       # Case insensitive tab completion
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"         # Colored completion (different colors for dirs/files/etc)
zstyle ':completion:*' rehash true                              # automatically find new executables in path 
# Speed up completions
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
HISTFILE=~/.zhistory
HISTSIZE=1000
SAVEHIST=500
WORDCHARS=${WORDCHARS//\/[&.;]}                                 # Don't consider certain characters part of the word
fpath=(~/.zsh/completions $fpath) 
autoload -Uz compinit
if [[ -n ~/.zcompdump(#qN.mh+24) ]]; then
  compinit
else
  compinit -C
fi

# pazi
if command -v pazi &>/dev/null; then
  eval "$(pazi init zsh)"
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

function create() {
  mkdir -p $1 && cd $1
}

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

# OPAM configuration
eval $(opam config env)
