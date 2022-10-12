#!/bin/bash
file="/tmp/.minimized"
touch $file
# By https://github.com/tatou-tatou
width=$(wattr w $(lsw -r))
height=$(wattr h $(lsw -r))
bar_width=$(( $width / 2 ))
left_shift=$(( ($width - $bar_width) / 2 ))
top_shift=$(( $height / 3 ))

case $1 in
hide)
    lines=$(wc -l < $file)

    if [[ $lines -ge 10 ]]; then ## Fight against bad practices
        notify-send "Ten windows are already hidden" "You should consider closing some before hiding even more." -i warning
    else
        focusedID=$(xdo id)
        if [[ "$focusedID" ]]; then
            focusedName=$(xdotool getwindowname $focusedID)
            focusedDesktop=$(xdotool get_desktop)

            echo "$focusedID $focusedDesktop $focusedName" >> $file && xdo hide $focusedID && notify-send "A window has been hidden (${lines})." "$focusedName" -i warning
        fi
    fi
    ;;
dmenu)
    miniList=$(cat $file)

    # Dmenu cannot draw more than 30 lines
    lines=$(wc -l < $file)
    if [[ $lines -gt 30 ]]
    then linesDisplayed=30
    else linesDisplayed=$lines
    fi

    # If the list is empty
    if [ -z "$miniList" ]
    then
        miniList="  Nothing is hidden!"
        linesDisplayed=1
    fi

    # Calculate where to draw the dmenu popup.
    # Comment that line if you use vanilla dmenu.
    yPos=$((410-$linesDisplayed*10))

    
    # Uncomment only one line with dmenu_cmd
    # If you use rofi
    dmenu_cmd="rofi -lines $linesDisplayed -dmenu -p Hidden:"
    
    # If you use dmenu, patched with eye candy
    # dmenu_cmd="dmenu -l 9 -x $left_shift -y $top_shift -w $bar_width -fn sans-11 -nb #201F1D -nf #eddec9 -sb #8F3724 -sf #EDDEC9 -p Hidden:"
        
    # If you use vanilla dmenu
    # dmenu_cmd="dmenu -b -i -l $linesDisplayed -p Hidden:"

    # Launch dmenu
    lineNumber=$(echo "$miniList" | cut -d " " -f 3- | nl -w 3 -n rn | sed -r 's/^([ 0-9]+)[ \t]*(.*)$/\1 - \2/' | $dmenu_cmd | cut -d '-' -f -1)

    # If you exited dmenu without selecting anything or if the list was empty
    [ -z "$lineNumber" -o "$miniList" = " Nothing is hidden!" ] && exit

    # Show the selected hidden window
    selectedID=$(sed -n "$lineNumber p" $file | cut -d ' ' -f 1)
    selectedDesktop=$(sed -n "$lineNumber p" $file | cut -d ' ' -f 2)
    xdotool set_desktop $selectedDesktop
    xdo show $selectedID && sed -i "${lineNumber}d" $file
    ;;
last)
    lines=$(wc -l < $file)
    selectedID=$(tail -n 1 $file | cut -d ' ' -f 1)
    selectedDesktop=$(sed -n "$lines p" $file | cut -d ' ' -f 2)
    xdotool set_desktop $selectedDesktop
    xdo show $selectedID && sed -i "${lines}d" $file
    ;;
esac
