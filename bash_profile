
if [ -x "/usr/bin/keychain" ]
then
	/usr/bin/keychain id_dsa
	. ~/.keychain/${HOSTNAME}-sh
fi

if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

