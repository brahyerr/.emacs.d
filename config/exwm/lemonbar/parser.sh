#!/usr/bin/env bash

cd $(dirname -- $0)

. ./common.sh
logo="\uf17c"

while read -r line; do
    case $line in
        DAT*)
            date="${line#???}"
            ;;
        BRI*)
            brightness="BRI: ${line#???}"
            ;;
        VOL*)
            volume="VOL: ${line#???}"
            ;;
        MEM*)
            memory="MEM: ${line#???}"
            ;;
        BAT*)
            battery="BAT: ${line#???}"
            ;;
        *) ;;
    esac

    echo -e "%{r}%{B${color_hl2}}    ${date}    ${brightness}    ${volume}    ${memory}    ${battery}    %{B-}"
done
