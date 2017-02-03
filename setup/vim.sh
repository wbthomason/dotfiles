# Fetches dein.vim and links init.vim to the appropriate place

mkdir -p ~/.config/nvim

curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh > installer.sh
sh ./installer.sh .config/nvim/dein
rm installer.sh

ln -s ~/.dotfiles/init.vim ~/.config/nvim/init.vim
