
function nwf {
    "$@"
    if [ $? -gt 0 ]; then
        notify-send -u critical "$1 FAILED"
    else
        notify-send -u critical "$1 finished"
    fi
}
