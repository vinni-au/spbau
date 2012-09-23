#!/bin/bash

function puts #x, y, string
{
	tput sc
	tput cup $1 $2
	echo "$3"
	tput rc
}

tput init
a=0;
for i in $(seq 0 23)
do
	for j in $(seq 0 79)
	do
		puts $i $j "*"
	done
done

