if [ $# -eq 0 ]
then
    EDGE="left"
else
    EDGE=$1
fi

echo xrandr --output $BUILT_IN_DISPLAY --auto
$(xrandr --output $BUILT_IN_DISPLAY --auto)

if [ "$CONNECTED_DISPLAY" != "$BUILT_IN_DISPLAY" ]
then
   echo xrandr --output $CONNECTED_DISPLAY --auto --$EDGE-of $BUILT_IN_DISPLAY
   $(xrandr --output $CONNECTED_DISPLAY --auto --$EDGE-of $BUILT_IN_DISPLAY)
fi
