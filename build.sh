#
if [ ! -f ~/.build/build ]; then
   if [ ! -d ~/.build ]; then
       mkdir ~/.build
   fi
   wget -O ~/.build/build https://arahant.com/files/build
   chmod 755 ~/.build/build
fi
~/.build/build "$@"
