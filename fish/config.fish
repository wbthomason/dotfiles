# Colors
set -xg fish_term24bit 1
if status --is-interactive
    eval sh $HOME/.config/base16-shell/scripts/base16-default-dark.sh
end

if [ $TERM = "konsole.*" ] 
    set -xg FZF_DEFAULT_OPTS "--color fg+:5,hl+:6 $FZF_DEFAULT_OPTS"
end

# Key bindings
fish_hybrid_key_bindings

# Opam
#source /home/wil/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
eval (opam config env)

# Rust
set -xg RUST_SRC_PATH '/usr/src/rust/src'

# Keychain
if status --is-interactive
    set -l IFS
    eval (keychain --eval --quiet -Q id_rsa)
end

# Environment variables
set -xg EDITOR 'vim'
set -xg PATH '/home/wil/.local/bin' $PATH
set -x fish_color_user 'b8bb26'
set -x fish_color_host '98bdc3'
set -xg TERM 'tmux-256color'

# Aliases
alias svn 'colorsvn'
alias eclimd '/usr/lib/eclipse/eclimd -b'
alias em 'emacsclient -nw'
alias emd 'emacs --daemon'
alias git 'hub'

# Tmux
if not set -q TMUX
    exec tmux new-session -A -s sysadmin
end

