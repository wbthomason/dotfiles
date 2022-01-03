# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Prompt
source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme

# Load plugins
export ZSH_PLUGINS=/usr/share/zsh/plugins/

source ${ZSH_PLUGINS}/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
source ${ZSH_PLUGINS}/zsh-autosuggestions/zsh-autosuggestions.zsh
source ${ZSH_PLUGINS}/zsh-history-substring-search/zsh-history-substring-search.zsh

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
HISTFILE=~/.zhistory
HISTSIZE=1000
SAVEHIST=500
setopt appendhistory
setopt histignorealldups
# Don't want common history between shells
unsetopt share_history

# Editor setting
export EDITOR=vim

# ROS
if [ -f /opt/ros/melodic/setup.zsh ]
then
  source /opt/ros/melodic/setup.zsh
fi

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
zstyle ':completion:*' completer _complete
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"         # Colored completion (different colors for dirs/files/etc)
zstyle ':completion:*' rehash true                              # automatically find new executables in path 
# Speed up completions
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
WORDCHARS=${WORDCHARS//\/[&.;]}                                 # Don't consider certain characters part of the word
fpath=(~/.zfunc ~/.zsh/completions $fpath) 
autoload -Uz compinit
if [[ -n ~/.zcompdump(#qN.mh+24) ]]; then
  compinit
  kitty + complete setup zsh | source /dev/stdin
else
  compinit -C
fi

# zoxide
if command -v zoxide &>/dev/null; then
  eval "$(zoxide init zsh)"
fi

# Thefuck
eval $(thefuck --alias)

# Aliases
alias svn='colorsvn'
alias eclimd='/usr/lib/eclipse/eclimd -b'
alias em='emacsclient -nw'
alias emd='emacs --daemon'
alias git='hub'
alias ls='ls --color=auto'

# Custom functions
function pdf_to_png() {
  convert -verbose -density 350 -trim $1 -quality 100 -flatten ${1%.pdf}.png
}

function create() {
  mkdir -p $1 && cd $1
}

function open() {
  xdg-open $1 > /dev/null 2>&1 &
}

# Auto-cd
setopt auto_cd

# Vi mode
bindkey -v
export KEYTIMEOUT=1
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

# FZF
export FZF_DEFAULT_COMMAND='rg --files --hidden --smart-case --glob "!.git/*"'
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh

bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down
bindkey '^[[A' history-substring-search-up			
bindkey '^[[B' history-substring-search-down

# OPAM configuration
test -r ~/.opam/opam-init/init.zsh && . ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# Keyring
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
  eval $(gnome-keyring-daemon --start --components=ssh,secrets,gpg)
  export SSH_AUTH_SOCK
fi

# Pyenv
eval "$(pyenv init -)"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
