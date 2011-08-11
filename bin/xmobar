#!/bin/bash

for PID in `pgrep xmobar`; do
    kill ${PID} > /dev/null &
done

/usr/bin/xmobar &
