#!/bin/bash

if [ $# -gt 0 ] 
then
	if [ $1 = "-b" ] 
	then
		grep -iE '/bin/bash$' /etc/passwd | grep -ioE '^[A-Z_]*' 
	fi
else
	grep -ioE '^[A-Z_]*' /etc/passwd
fi
