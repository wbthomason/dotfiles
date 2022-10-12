#!/bin/bash
# Originally by https://github.com/windelicato/

size=${2:-'20'}
dir=$1

transplanter() {
	bspc node ${dir} -p south && bspc node -n ${dir}
}

northplanter() {
	bspc node north -p north && bspc node -n north
}

rootplanter() {
	bspc node @/ -p ${dir} && bspc node -n @/ || bspc node -s next.local && bspc node -n @/
	bspc node @/ -p cancel
}

bspc config pointer_follows_focus true
# Find current window mode
is_floating() {
bspc query -T -n | grep -q '"state":"floating"'
}
# If the window is floating, move it
if is_floating; then
#only parse input if window is floating,tiled windows accept input as is
        case "$dir" in
  		west) switch="-x"
  		sign="-"
        	;;
  		east) switch="-x"
         	sign="+"
       		;;
  		north) switch="-y"
         	sign="-"
        	;;
  		*) switch="-y"
     		sign="+"
     		;;
 		esac
 xdo move ${switch} ${sign}${size}

# Otherwise, window is tiled: switch with window in given direction
else
	 if [[ $(bspc query -N -n .local.\!floating | wc -l) != 2 ]]; then 
	 case "$dir" in
  		north) northplanter || rootplanter
        	;;
  		*) transplanter || rootplanter
     		;;
 	 esac
 	 else
 	 case "$dir" in
  		east) bspc node -s east || bspc query -N -n west.local || \
        if bspc query -N -n south.local ; then
        	bspc node @/ -R 90
        	else
        	bspc node @/ -R 270
        fi
        	;;
        west) bspc node -s west || bspc query -N -n east.local || \
        if bspc query -N -n north.local ; then
        	bspc node @/ -R 90
        	else
        	bspc node @/ -R 270
        fi
        	;;
        south) bspc node -s south || bspc query -N -n north.local || \
        if bspc query -N -n west.local ; then
        	bspc node @/ -R 90
        	else
        	bspc node @/ -R 270
        fi 
        	;;
  		*) bspc node -s north || bspc query -N -n south.local || \
        if bspc query -N -n west.local ; then
        	bspc node @/ -R 270
        	else
        	bspc node @/ -R 90
        fi
     		;;
     esac
     fi
fi
