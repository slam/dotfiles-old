
if [ -x "/usr/bin/keychain" ]
then
	eval `/usr/bin/keychain --eval id_dsa`
fi


if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

bashprofile_arch=~/.bash/bash_profile.`uname`
if [ -f $bashprofile_arch ] ; then
	. $bashprofile_arch
fi

if [ -f ~/.bash/bash_profile.local ] ; then
	. ~/.bash/bash_profile.local
fi

