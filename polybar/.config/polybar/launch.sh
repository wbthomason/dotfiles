#!/usr/bin/env sh

## Add this to your wm startup file.

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch bars
PRIMARY=$(xrandr --query | grep " connected" | grep "primary" | cut -d" " -f1)
for i in $(polybar -m | awk -F: '{print $1}'); do 
  TRAY_POS=$([[ "$i" == "$PRIMARY" ]] && echo "right" || echo "") MONITOR=$i polybar -c ~/.config/polybar/config.ini main &
done
