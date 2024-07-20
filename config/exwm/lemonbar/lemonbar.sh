#!/usr/bin/env bash

cd $(dirname -- $0)

. ./common.sh

fifo=${XDG_RUNTIME_DIR:-/tmp}/lemonbar.fifo
test -e $fifo && rm $fifo
mkfifo $fifo

trap 'pkill lemonbar; kill $(jobs -p); emacsclient --eval "(local/kill-panel)"' EXIT

datetime() {
    date "+DAT%a %d %b, %I:%M %p" > $fifo
}

# net() {
#     # !!! Unfinished !!!
#     dev=$(ip route show | cut -d ' ' -f 5 | head -n 1)
# }

# up-time(){
#     upsec="$( cat /proc/uptime |awk '{print $1}' |cut -d'.' -f1)"
#     d=$((${upsec}/(60*60*24)))
#     h=$(((${upsec}%(60*60*24))/(60*60)))
#     m=$(((${upsec}%(60*60))/60))
#     echo $(printf "%s%02dd:%02dh:%02dm\n" "UPT" $d $h $m) > $fifo
# }

load(){
    load1=$(cat /proc/loadavg |awk '{print $1 " " $2  " " $3}')
    # printf "%s%s\n" "LOD" "${load1}"
    echo "LOD${load1}" > $fifo
}

cpu(){
    A=($(sed -n '1p' /proc/stat))
    # user         + nice     + system   + idle
    B=$((${A[1]} + ${A[2]} + ${A[3]} + ${A[4]}))
    sleep .2
    C=($(sed -n '1p' /proc/stat))
    # user         + nice     + system   + idle
    D=$((${C[1]} + ${C[2]} + ${C[3]} + ${C[4]}))
    # cpu usage
    E=$(((100 * (B - D - ${A[4]} + ${C[4]})) / (B - D)))
    # printf "CPU%s%%\n" "$(printf '%02d' $E)"
    echo "CPU$(printf '%02d' $E)%" > $fifo
}

brightness() {
    echo "BRI$(brightnessctl -m | cut -d ',' -f 4)" > $fifo
}

volume() {
    current=$(wpctl get-volume @DEFAULT_SINK@)
    current_n=$(echo "$(cut -d ' ' -f 2 <<< $current) * 100" | bc | cut -d '.' -f '1')
    if [[ "$(cut -d ' ' -f 3 <<< $current)" == "[MUTED]" ]]; then
	current_n="[MUTED]"
    fi
    echo "VOL${current_n}%" > $fifo
}

memory() {
    echo "MEM$(free -m | awk 'NR==2 {print $3 " /", $2 " MB"}')" > $fifo
}

battery() {
    echo "BAT$(cat /sys/class/power_supply/BATT/capacity)%" > $fifo
}

# Initialize bar data
datetime &
load &
cpu &
brightness &
volume &
memory &
battery &

# Poll
while :; do datetime; sleep 5s; done &
while :; do load; sleep 3s; done &
while :; do cpu; sleep 2s; done &
while :; do memory; sleep 3s; done &
while :; do battery; sleep 20s; done &

tail -f $fifo | $(dirname -- $0)/parser.sh | lemonbar \
	-p \
	-g "x20+0+0" \
	-B "${color_bg}" \
	-F "${color_fg}" \
	-f "monospace:size=9:weight=semibold"
