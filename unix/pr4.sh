#!/bin/bash

function puts #x, y, string
{
	tput sc
	tput cup $1 $2
	echo -ne "\033[47m \033[0m"
	tput rc
}

tput init
a=0;

left=0
right=`tput cols`
right=`expr $right - 1`
top=0
bottom=`tput lines`
bottom=`expr $bottom - 2`

for i in $(seq $left $right)
do
	puts $top $i "*"
done

for i in $(seq $top $bottom)
do
	puts $i $right "*"
done

for i in $(seq $right -1 $left)
do
	puts $bottom $i "*"
done

top=`expr $top + 2`

while ( true )
do

	if [ $bottom -le `expr $top + 1` ]; then
		exit
	fi

	for i in $(seq $bottom -1 $top)
	do
		puts $i $left "*"
	done

	right=`expr $right - 2`

	if [ $left -ge `expr $right - 1` ]; then
		exit
	fi

	for i in $(seq $left $right)
	do
		puts $top $i "*"
	done

	bottom=`expr $bottom - 2`

	if [ $top -ge `expr $bottom - 1` ]; then
		exit
	fi

	for i in $(seq $top $bottom)
	do
		puts $i $right "*"
	done

	left=`expr $left + 2`

	if [ $left -ge `expr $right - 1` ]; then
		exit
	fi
	for i in $(seq $right -1 $left)
	do
		puts $bottom $i "*"
	done

	top=`expr $top + 2`

done
