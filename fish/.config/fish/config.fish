# Colors
set -xg fish_term24bit 1
# if status --is-interactive
#     eval sh /home/wil/.config/base16-shell/scripts/base16-tomorrow-night.sh
# end

if [ $TERM = "konsole.*" ]
    set -xg FZF_DEFAULT_OPTS "--color fg+:5,hl+:6 $FZF_DEFAULT_OPTS"
end

# Key bindings
fish_hybrid_key_bindings

# Environment variables
set -xg EDITOR 'vim'
set -xg PATH '/home/wil/.local/bin' '/home/wil/.cargo/bin' '/home/wil/.luarocks/bin' $PATH
set -x fish_color_user 'b8bb26'
set -x fish_color_host '98bdc3'
# This is because we still want zsh as the "default" shell, but we want to override it for
# shell-dependent env config later
set -xg SHELL '/usr/bin/fish'

# Opam
eval (opam config env)

# Rust
set -xg RUST_SRC_PATH '/usr/src/rust/src'

# Keychain
if status --is-interactive
    set -l IFS
    eval (keychain --agents ssh --eval --quiet -Q id_rsa)
    eval (keychain --agents gpg --eval --quiet -Q 8BEE5C508226C4516876B93449B3E4A714642E1D)
end

# Aliases
alias svn 'colorsvn'
alias eclimd '/usr/lib/eclipse/eclimd -b'
alias em 'emacsclient -nw'
alias emd 'emacs --daemon'
alias git 'hub'

# ROS
bass source /opt/ros/lunar/setup.bash
set -xg ROS_HOSTNAME localhost
set -xg ROS_MASTER_URI http://localhost:11311

# Tmux
if not set -q TMUX
    exec tmux new-session -A -s sysadmin
end
