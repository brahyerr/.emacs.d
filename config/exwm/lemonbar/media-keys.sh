#!/usr/bin/env bash

fifo=${XDG_RUNTIME_DIR:-/tmp}/lemonbar.fifo
test -e $fifo || echo "Error! no fifo present"

volume() {
    current=$(wpctl get-volume @DEFAULT_SINK@)
    current_n="$(echo "$(cut -d ' ' -f 2 <<< $current) * 100" | bc | cut -d '.' -f '1')%"
    if [[ "$(cut -d ' ' -f 3 <<< $current)" == "[MUTED]" ]]; then
	current_n="[MUTED]"
    fi
    echo "VOL${current_n}" > $fifo
}

brightness() {
    echo "BRI$(brightnessctl -m | cut -d ',' -f 4)" > $fifo
}

if [[ "$1" == "vol" ]]; then
    volume &
elif [[ "$1" == "bri" ]]; then
    brightness &
fi
