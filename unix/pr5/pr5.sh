#!/bin/bash

function puts #x, y, string
{
	tput sc
	tput cup $1 $2
#	echo -ne "\033[47m \033[0m"
	echo -e $3
	tput rc
}

tput init

n=$(echo -n `cat in.txt | grep -oE "[0-9]+" | head -1`)
m=$(echo -n `cat in.txt | grep -oE "[0-9]+" | head -2 | tail -1`)
x=$(echo -n `cat in.txt | tail -2 | grep -oE "[0-9]+" | head -1`)
y=$(echo -n `cat in.txt | tail -2 | grep -oE "[0-9]+" | head -2 | tail -1`)

k=$(echo -n `cat in.txt | tail -2 | grep -oE "[0-9]+" | head -3 | tail -1`)
l=$(echo -n `cat in.txt | tail -2 | grep -oE "[0-9]+" | head -4 | tail -1`)

for (( i=1; i <= $n; i++ ))
do
	s=`expr $i + 1`
	str=$(echo -n `cat in.txt | head -$s | tail -1`)
	arr[i]="$str"
done

function draw
{
for ((i=1; i <= "$n"; i++))
do
	cur=${arr[$i]}
	for ((j=0; j < "$m"; j++))
	do
		if [ ${cur:$j:1} = "1" ] ; then
			puts $j $i "\033[47m \033[0m"
		else
			if [ ${cur:$j:1} -gt 0 ]; then
				puts $j $i "${cur:$j:1}"
			fi
		fi
	done
done
}

y=`expr $y - 1`
l=`expr $l - 1`

draw

if [[ $x = $k && $y = $l ]]; then
	puts $y $x "O"
	exit
else
	puts $y $x "S"
	puts $l $k "F"
fi

#while (( 1 ))
#do
#	draw
#done
