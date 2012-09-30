#!/bin/bash

ask=true

if [ $# -eq 1 ];	then
	if [ $1 = -help -o $1 = -? ];	then
		echo "smart delete"
		echo "usage:"
		echo " -help | -? | -v \"mask\" | \"mask\""
		echo " -?, -help - show this help"
		echo " -v - supress questions"
		exit
	fi
fi

if [ $# -eq 2 -a $1 = "-v" ];	then
	ask=false
else
	if [ $1 = "-v" ]
	then
		ask=false
	fi
fi

if [ $# -eq 2 ]; then
	mask=${2//\*/\*}
else
	mask=${1//\*/\*}
fi

for file in `find $mask`; do
	if [ $ask = "true" ];	then
		read -p "delete file $file? (yes/no)" yn
		case $yn in
			yes* ) ;;
			no* ) continue;;
			* ) continue;;
		esac
	fi
	echo "deleting $file"
	rm -f $file
done
