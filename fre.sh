printf "source $HOME/Config/ba.sh\n" >> $HOME/.bashrc

mkdir $HOME/.emacs.d/
touch $HOME/.emacs.d/local.el
ln -s $HOME/Config/init.el $HOME/.emacs.d/init.el

mkdir -p $HOME/.config/i3/
ln $HOME/Config/i3-config $HOME/.config/i3/config

mkdir -p $HOME/.config/i3status/
ln $HOME/Config/i3-status-config $HOME/.config/i3status/config

