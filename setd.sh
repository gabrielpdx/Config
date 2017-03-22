NOW_CONNECTED=$(xrandr | grep ' connected' | grep -o '^[a-zA-Z0-9-]*' | head -n 2 | tail --lines=1)
if [ "$NOW_CONNECTED" != "eDP1" ]
then	 
		if [ "$NOW_CONNECTED" != "$CONNECTED_DISPLAY" ]
		then
				$(xrandr --output $CONNECTED_DISPLAY --auto)
		fi
		export CONNECTED_DISPLAY=$NOW_CONNECTED
fi

