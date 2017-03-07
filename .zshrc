# Path to your oh-my-zsh installation.
ZSH=/usr/share/oh-my-zsh/

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="agnoster"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"
ZSH_CACHE_DIR=$HOME/.oh-my-zsh-cache
if [[ ! -d $ZSH_CACHE_DIR ]]; then
  mkdir $ZSH_CACHE_DIR
fi
# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="mm.dd.yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(gitfast archlinux cabal systemd coffee docker npm pip python svn tmux)

# User configuration

#export PATH="/usr/local/opt/coreutils/libexec/gnubin:/usr/local/bin:/opt/local/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/texbin:/opt/anaconda/bin"
# export MANPATH="/usr/local/man:$MANPATH"
#export PATH="/usr/local/bin:/usr/local/sbin:$PATH:~/bin"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
 if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='vim'
 else
   export EDITOR='vim'
 fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias svn=colorsvn
alias ls="ls --color=auto"
eval $(dircolors -b)
alias eclimd="/usr/lib/eclipse/eclimd -b"
alias fuck='eval $(thefuck $(fc -ln -1 | tail -n 1)); fc -R'
alias em="emacsclient -nw"
alias emd="emacs --daemon"
# Powerline stuff
#source /usr/share/zsh/site-contrib/powerline.zsh
powerline-daemon -q
. /usr/lib/python3.6/site-packages/powerline/bindings/zsh/powerline.zsh

# OPAM configuration
export MONO_GAC_PREFIX="/usr/local"
source /usr/share/nvm/nvm.sh
ulimit -n 1000

# OPAM configuration
. /home/wil/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
eval `opam config env`

eval `keychain --quiet --eval ~/.ssh/id_rsa`
eval "$(hub alias -s)"
fpath=(~/.zsh/completions $fpath)
autoload -U compinit && compinit

LS_COLORS=$LS_COLORS:'di=0;36:ex=0;32:' ; export LS_COLORS
export RUST_SRC_PATH=/usr/src/rust/src

export PATH=/home/wil/.local/bin:$PATH
export DISPLAY=:0.0

[[ $- != *i* ]] && return
PARENT=`ps -p $PPID -o comm=`
if [[ -z "$TMUX" && ! $PARENT =~ emacs ]]; then
    exec tmux new-session -A -s sysadmin
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
