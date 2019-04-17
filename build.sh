#
# This shell script bootstraps the BUILD system located at:
# https://github.com/blakemcbride/Build
# Written by Blake McBride

if [ ! -f ~/.build/build ]; then
   if [ ! -d ~/.build ]; then
       mkdir ~/.build
   fi
   wget -O ~/.build/build https://arahant.com/files/build
   chmod 755 ~/.build/build
fi
~/.build/build "$@"
