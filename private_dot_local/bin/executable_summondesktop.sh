#! /bin/sh
#in sxhkdrc call it with
#alt + {1-9,0}
#     summonworkspace.sh {1-9,0}
  D=$(bspc query -D -m | sed -n "$@ p"); \
  M=$(bspc query --monitors --desktop $D); \
  if [ $(bspc query --desktops --monitor $M | wc -l) -gt 1 ]; then \
    if [ $(bspc query --desktops --desktop focused) != $D ]; then \
      bspc desktop $D --to-monitor focused; \
      bspc desktop $D --focus; \
    fi; \
  elif [ $(bspc query --monitors --monitor focused) != $M ]; then \
    bspc desktop $(bspc query --monitors --desktop $D):focused --swap $(bspc query -M -m focused):focused; \
  fi
