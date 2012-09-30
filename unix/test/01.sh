#!/bin/bash

fmt="%T"

if [ $# -gt 0 ]
then
	if [ $1 = "-d" ] 
	then
		fmt="%d-%m-%Y %T"
	fi
fi

while (( 1 )) 
do
	date +"$fmt"
	echo -ne "\033[1A"
	sleep 1
done
