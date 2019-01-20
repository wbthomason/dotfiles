# Load plugins
export ZSH_AUTOSUGGEST_USE_ASYNC=1
fpath=(/usr/local/share/zsh-completions $fpath)
source /usr/local/share/zsh-autosuggestions/zsh-autosuggestions.zsh

source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

source /usr/local/share/zsh-history-substring-search/zsh-history-substring-search.zsh
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
export PATH=$PATH:/home/wil/.local/bin:/home/wil/.cargo/bin:/home/wil/.luarocks/bin

# Editor setting
export EDITOR=vim

# FZF
export FZF_DEFAULT_COMMAND='rg --files --hidden --smart-case --glob "!.git/*"'

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

# Aliases
alias svn='colorsvn'
alias eclimd='/usr/lib/eclipse/eclimd -b'
alias em='emacsclient -nw'
alias emd='emacs --daemon'
alias git='hub'
alias ls='ls -G'

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
