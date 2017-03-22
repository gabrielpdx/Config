alias l1="ls -1"
alias hig="history | cut -c 8- | grep"
alias f="find . -iname"
alias notes="cd $HOME/Notes/"
alias movgif="ffmpeg -i in.mov -s 600x400 -r 10 -f gif - | gifsicle --optimize=3 --delay=3 > out.gif"
alias hgee="hg branches && hg status"

function wot() {
    grep -e '^alias ' $HOME/.bashrc
    grep -e '^alias ' $HOME/Config/ba.sh
    awk '/function /,/}$/' $HOME/.bashrc
    awk '/function /,/}$/' $HOME/Config/ba.sh
}

function dis() {
    source $HOME/Config/setd.sh
    $HOME/Config/addd.sh $1
}

