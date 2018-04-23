export PATH=/home/wil/.local/bin:$PATH
export RUST_SRC_PATH=/usr/src/rust/src
ZSH=/usr/share/oh-my-zsh/
source $ZSH/oh-my-zsh.sh
source /opt/ros/lunar/setup.zsh
#[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# OPAM configuration
export MONO_GAC_PREFIX="/usr/local"
ulimit -n 1000

# OPAM configuration
#. /home/wil/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
#eval `opam config env`

# Go config
#export PATH=$PATH:/home/wil/go/bin/
