export PS1="\e[36m(((\u@\h \W)))\$ \e[m "

alias l1="ls -1"
alias f="find . -iname"
alias notes="cd $HOME/Notes/"
alias movgif="ffmpeg -i in.mov -s 600x400 -r 10 -f gif - | gifsicle --optimize=3 --delay=3 > out.gif"
alias hgee="hg branches && hg status"

source $HOME/.localbashrc

alias wot="grep -e '^alias' $HOME/.bashrc && grep -e '^alias' $HOME/.localbashrc"

