#!/bin/bash

state=$(cat /sys/class/mic/mic0/state)
if [[ "'$state'" != "'resetting'" ]] && [[ "'$state'" != "'ready'" ]]; then
  micctrl -r mic0
  sleep 1;
fi

