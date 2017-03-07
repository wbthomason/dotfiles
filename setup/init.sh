#!/bin/sh

# This script is designed to set up a new Arch Linux machine according to my existing configurations
# and tools. We can assume that we have wget installed, and about only that.

echo "Beginning new machine setup..."

# Install git

echo "Installing git..."
sudo pacman -S git
echo "git installed"

# Fetch .dotfiles

echo "Fetching configuration files..."
git clone git://github.com/wbthomason/.dotfiles
cd .dotfiles
git checkout linux
cd ~
echo "Done fetching configuration files"

# Get Yaourt set up

echo "Installing yaourt..."
. setup/yaourt.sh
echo "Done installing yaourt"

# Add Haskell repo

echo "Add the Haskell community repo..."
. setup/haskell.sh
echo "Haskell repo added"

# Install packages

echo "Beginning package installation..."
. setup/packages.sh
echo "Finished package installation"

# Get vim set up

echo "Beginning vim plugin installation..."
. setup/vim.sh
echo "Done setting up vim"

# Get Spacemacs set up

echo "Fetching Spacemacs..."
. setup/emacs.sh
echo "Done getting Spacemacs"

# Get tmux set up

echo "Fetching tmux config..."
. setup/tmux.sh
echo "Finished setting up tmux"

# Make links to remaining configuration files

echo "Linking config files..."
. setup/config.sh
echo "Done linking config files"
