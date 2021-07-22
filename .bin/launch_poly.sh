#!/usr/bin/env zsh

# More info : https://github.com/jaagr/polybar/wiki

# Install the following applications for polybar and icons in polybar if you are on ArcoLinuxD
# awesome-terminal-fonts
# Tip : There are other interesting fonts that provide icons like nerd-fonts-complete
# --log=error
# Terminate already running bar instances
echo "running polybar script"
echo "killing existing bars..."
killall -q polybar
echo "killed"

echo "Wait until the processes have been shut down"
while pgrep -u $UID -x polybar > /dev/null; do sleep 1; done

m=$(xrandr --query | grep " connected" | cut -d" " -f1)
echo "1 monitor is: $m"
MONITOR=$m polybar --reload mainbar-xmonad -c ~/.config/polybar/config &
