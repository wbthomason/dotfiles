# Sets up config files

# i3
mkdir -p ~/.config/i3
ln -s ~/.dotfiles/i3-config ~/.config/i3/config

# Backgrounds
ln -s ~/.dotfiles/desktopbgs ~/desktopbgs
ln -s ~/.dotfiles/bgscript.sh ~/bgscript.sh 

# Ctags
ln -s ~/.dotfiles/.ctags ~/.ctags

# Git
ln -s ~/.dotfiles/.gitconfig ~/.gitconfig
ln -s ~/.dotfiles/.gitignore_global ~/.gitignore_global

# Polybar
ln -s ~/.dotfiles/polybar ~/.config/polybar

# Termite
mkdir -p ~/.config/termite
ln -s ~/.dotfiles/termite-config ~/.config/termite/config

# Zsh
ln -s ~/.dotfiles/.zshrc ~/.zshrc
