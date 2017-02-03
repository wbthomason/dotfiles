# Fetches tmux config base and links custom config

git clone https://github.com/gpakosz/.tmux.git
ln -s -f ~/.tmux/.tmux.conf
ln -s ~/.dotfiles/.tmux.conf.local ~/.tmux.conf.local
