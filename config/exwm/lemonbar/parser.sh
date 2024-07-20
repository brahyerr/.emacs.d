#!/usr/bin/env bash

cd $(dirname -- $0)

. ./common.sh
logo="\uf17c"

while read -r line; do
    case $line in
        WIN*)
            win="EXWM: ${line#???}"
            ;;
        NAM*)
            name="${line#???}"
            ;;
        DAT*)
            date="${line#???}"
            ;;
        BRI*)
            brightness="BRI: ${line#???}"
            ;;
        VOL*)
            volume="VOL: ${line#???}"
            ;;
        CPU*)
            cpu="CPU: ${line#???}"
            ;;
        LOD*)
            lod="LOD: ${line#???}"
            ;;
        MEM*)
            memory="MEM: ${line#???}"
            ;;
        BAT*)
            battery="BAT: ${line#???}"
            ;;
        *) ;;
    esac
    echo -e "%{l}%{B${color_hl2}}   ${date}   /   ${win}   /   ${name}   %{B-}%{r}%{B${color_hl2}}   ${brightness}   /   ${volume}   /   ${cpu}   /   ${lod}   /   ${memory}   /   ${battery}   %{B-}"
done
