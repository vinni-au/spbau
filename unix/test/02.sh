#!/bin/bash

for ((i=0; i < 8; i++))
do
	x=`expr $i % 2`
	if [ $x -eq 0 ]
	then
		echo -ne "  "
	fi
	for ((j=0; j < 4; j++))
	do
		echo -ne "\033[47m  \033[0m  "
	done
	echo ""
done
