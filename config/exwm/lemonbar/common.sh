#!/usr/bin/env bash

theme="ef-spring"

case $theme in
    "dracula")
        color_bg="#282a36"
        color_fg="#f8f8f2"
        color_hl1="#6272a4"
        color_hl2="#bd93f9"
        ;;
    "ef-spring")
	color_bg="#f6fff9"
	color_fg="#34494a"
	color_fg_alt="#e8f0f0"
        color_hl1="#90e8b0"
        color_hl2="#e8f0f0"
        color_hl3="#777294"
	color_hl4="#cb26a0"
	;;
    *) # Default to Nord
        color_bg="#2e3440"
        color_fg="#eceff4"
        color_hl1="#5e81ac"
        color_hl2="#81a1c1"
        ;;
esac
