cd
. /Users/sahn/.bash_profile
NEW_DISPLAY=`ls -t /tmp/launch-*/org.macosforge.xquartz* | head -n 1`
if [ "x$NEW_DISPLAY" != "x" ]
then
  export DISPLAY=$NEW_DISPLAY
  echo "Setting DISPLAY to $DISPLAY"
fi
exec /usr/local/bin/xmonad
