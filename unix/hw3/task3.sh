#!/bin/bash

if [ "`id -u`" -eq 0 ] 
then
	echo "Hi, root! Have a nice day!";
else
	echo "Hi, `whoami`!";
fi

if [ $SHELL == "/bin/bash" ]
then
	echo "You're using good shell!";
else
	echo "(\___/)";
	echo "(='.'=)";
	echo "(\")_(\")";
fi
