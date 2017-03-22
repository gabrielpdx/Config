# now put it in your i3 config and set up in git :^)
if [ $# -eq 0 ]
  then
			EDGE="left"
else
		EDGE=$1
fi

echo xrandr --output eDP1 --auto
$(xrandr --output eDP1 --auto)
echo xrandr --output $CONNECTED_DISPLAY --auto --$EDGE-of eDP1
$(xrandr --output $CONNECTED_DISPLAY --auto --$EDGE-of eDP1)
