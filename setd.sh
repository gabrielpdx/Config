BUILT_IN=$(xrandr | grep ' connected' | grep -o '^[a-zA-Z0-9-]*' | head -n 1)
NOW_CONNECTED=$(xrandr | grep ' connected' | grep -o '^[a-zA-Z0-9-]*' | head -n 2 | tail --lines=1)
if [ "$NOW_CONNECTED" != "$BUILT_IN" ]
then
    if [ "$NOW_CONNECTED" != "$CONNECTED_DISPLAY" ]
    then
        $(xrandr --output $CONNECTED_DISPLAY --auto)
    fi
    export CONNECTED_DISPLAY=$NOW_CONNECTED
fi
export BUILT_IN_DISPLAY=$BUILT_IN

