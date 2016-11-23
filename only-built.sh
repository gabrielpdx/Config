ln -s $HOME/Config/init.el $HOME/.emacs.d/init.el
ln -s $HOME/Config/.bashrc $HOME/.bashrc
ln -s $HOME/Config/pom $HOME/bin/pom
rm $HOME/.bash_profile && echo "source $HOME/.bashrc" > $HOME/.bash_profile
