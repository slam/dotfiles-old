
if [ -x "/usr/bin/keychain" ]
then
	/usr/bin/keychain id_dsa
	. ~/.keychain/${HOSTNAME}-sh
fi

if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

bashprofile_arch=~/.bash/bash_profile.`uname`
if [ -f $bashprofile_arch ] ; then
	. $bashprofile_arch
fi
