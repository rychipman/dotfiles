# make sure logdir exists
logdir=~/.local/share/bspwm
mkdir -p "$logdir/old-logs"

# clean up old logs
bakdir="$logdir/old-logs/$(date +"%Y_%m_%d_%H_%M_%S").old"
mkdir $bakdir
mv $logdir/*.log $bakdir
ln -sfT $bakdir $logdir/logs-last
rm $logdir/*.fifo

# set keymap
~/bin/set-keymap

# start dunst
dunst > $logdir/dunst.log 2>&1  &

# start redshift
redshift > $logdir/redshift.log 2>&1 &

# start unclutter
unclutter > $logdir/unclutter.log 2>&1 &

# start sxhkd
sxhkd > $logdir/sxhkd.log 2>&1 &

# start bspwm
exec bspwm > $logdir/bspwm.log 2>&1
