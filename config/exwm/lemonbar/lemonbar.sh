#!/usr/bin/env bash

cd $(dirname -- $0)

. ./common.sh

fifo=${XDG_RUNTIME_DIR:-/tmp}/lemonbar.fifo
test -e $fifo && rm $fifo
mkfifo $fifo

trap 'pkill lemonbar; kill $(jobs -p)' EXIT

# Date
while :; do
	date "+DAT%a %d %b, %H:%M" > $fifo
    sleep 5;
done &

# Brightness
while :; do
    echo "BRI$(brightnessctl -m | cut -d ',' -f 4)" > $fifo
    sleep 2
done &

# Volume
while :; do
    current=$(wpctl get-volume @DEFAULT_SINK@)
    current_n=$(echo "$(cut -d ' ' -f 2 <<< $current) * 100" | bc | cut -d '.' -f '1')
    # current_n=$(cut -d'%' -f1 <<< $current)

    if [[ "$(cut -d ' ' -f 3 <<< $current)" == "[MUTED]" ]]; then
	current_n="[MUTED]"
        # icon="\ueee8"
    # elif [ $current_n -gt 50 ]; then
    #     icon="\uf028"
    # elif [ $current_n -gt 25 ]; then
    #     icon="\uf027"
    # else
    #     icon="\uf026"
    fi

    echo "VOL${current_n}%" > $fifo

    sleep 2
done &

# Memory
while :; do
    echo "MEM$(free -m | awk 'NR==2 {print $3 " /", $2 " MB"}')" > $fifo
    sleep 5
done &

# Battery
while :; do
    echo "BAT$(cat /sys/class/power_supply/BATT/capacity)%" > $fifo
    sleep 10
done &

tail -f $fifo | $(dirname -- $0)/parser.sh | lemonbar \
	-p \
	-g "x20+0+0" \
	-B "${color_bg}" \
	-F "${color_fg}" \
	-f "IBM Plex Mono:size=9:weight=semibold"
