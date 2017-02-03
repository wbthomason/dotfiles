# Fetches, builds, and installs yaourt

echo "Getting package-query..."
mkdir pq-build
cd pq-build
wget -O PKGBUILD https://aur.archlinux.org/cgit/aur.git/plain/PKGBUILD?h=package-query
makepkg -si
cd ~
rm -rf pq-build

echo "Getting yaourt..."
mkdir yaourt-build
cd yaourt-build
wget -O PKGBUILD https://aur.archlinux.org/cgit/aur.git/plain/PKGBUILD?h=yaourt
makepkg -si
cd ~
rm -rf yaourt-build
