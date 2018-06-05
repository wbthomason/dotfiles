# pkg-config tweaks
set -xg PKG_CONFIG_PATH "$PKG_CONFIG_PATH:/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig"

# LD_LIBRARY_PATH tweaks
set -xg LD_LIBRARY_PATH "$LD_LIBRARY_PATH:/usr/local/lib:/usr/local/lib64"

# Colors
set -xg fish_term24bit 1
# if status --is-interactive
#     eval sh /home/wil/.config/base16-shell/scripts/base16-tomorrow-night.sh
# end

# Key bindings
fish_hybrid_key_bindings

# Environment variables
set -xg EDITOR 'vim'
set -xg PATH $PATH '/home/wil/.local/bin' '/home/wil/.cargo/bin' '/home/wil/.luarocks/bin' '/home/wil/.roswell/bin'
set -x fish_color_user 'b8bb26'
set -x fish_color_host '98bdc3'
# This is because we still want zsh as the "default" shell, but we want to override it for
# shell-dependent env config later
set -xg SHELL '/usr/bin/fish'

# Opam
eval (opam config env)

# Rust
set -xg RUST_SRC_PATH '/usr/src/rust/src'

# gnome-keyring
if test -n "$DESKTOP_SESSION"
    # set -l IFS
    # eval (keychain --agents ssh --eval --quiet id_rsa)
    # eval (keychain --agents gpg --eval --quiet 8BEE5C508226C4516876B93449B3E4A714642E1D)
    set -xg SSH_AUTH_SOCK (gnome-keyring-daemon --start --components=gpg,ssh,secrets | awk -F= '{print  $2}')
end

# Aliases
alias svn 'colorsvn'
alias eclimd '/usr/lib/eclipse/eclimd -b'
alias em 'emacsclient -nw'
alias emd 'emacs --daemon'
alias git 'hub'

# ROS
bass source /opt/ros/melodic/setup.bash
set -xg ROS_HOSTNAME localhost
set -xg ROS_MASTER_URI http://localhost:11311

# fasd
alias a 'fasd -a'        # any
alias s 'fasd -si'       # show / search / select
alias d 'fasd -d'        # directory
alias f 'fasd -f'        # file
alias sd 'fasd -sid'     # interactive directory selection
alias sf 'fasd -sif'     # interactive file selection
alias z 'fasd_cd -d'     # cd, same functionality as j in autojump
alias zz 'fasd_cd -d -i' # cd with interactive selection

# Tmux
if not set -q TMUX
    exec tmux new-session -A -s sysadmin
end
