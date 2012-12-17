#!/bin/bash

if [ $# -ne 1 ]; then
	echo "use: hanoi.sh N"
	echo "N is a disk count"
	echo "N should be greater than 1 and less than 11"
	exit 0
fi

N=$1 
if [ $N -gt 10 ]; then
	echo "Error: N>10"
	exit -1
fi

if [ $N -lt 1 ]; then
	echo "Error: N<1"
	exit -1
fi


S=1
count1=$N
count2=0
count3=0

function puts { # row, col, string
	tput sc
	tput cup $1 $2
	echo -ne "$3"
	tput rc
}

function printbars {
	for col in $(seq 12 26 69);	do
		for row in $(seq 0 10);	do
			puts $row $col "|"
		done
	done
	for col in $(seq 0 79); do
		puts 11 $col "-"
	done
}

function printdisk { # barnum, size, level
	row=$(expr 11 - $3)
	col=$(expr 11 + `expr 26 \* $1`)
	for c in $(seq $col -1 `expr $col - $2`);	do
		puts $row $c "\033[47m \033[0m"
	done
	col=$(expr $col + 2)
	for c in $(seq $col `expr $col + $2`);	do
		puts $row $c "\033[47m \033[0m"
	done
}


function cleardisk { # barnum, size, level
	row=$(expr 11 - $3)
	col=$(expr 11 + `expr 26 \* $1`)
	for c in $(seq $col -1 `expr $col - $2`);	do
		puts $row $c " "
	done
	col=$(expr $col + 2)
	for c in $(seq $col `expr $col + $2`);	do
		puts $row $c " "
	done
}

function movedisk { #barnum, size, level, newbarnum, newlevel
	cleardisk $1 $2 $3
	printdisk $4 $2 $5
}

function domove {
	to=`expr $2 - 1`
	if [ $1 -eq 1 ]; then	
		disksize=${first[$count1]};	level=$count1;	first[$count1]=0; fi
	if [ $1 -eq 2 ]; then 	
		disksize=${second[$count2]};level=$count2;	second[$count2]=0; fi
	if [ $1 -eq 3 ]; then 	
		disksize=${third[$count3]}; level=$count3;	third[$count3]=0; fi

	if [ $2 -eq 1 ]; then 
		count1=`expr $count1 + 1`; newlevel=$count1; 
		first[$count1]=$disksize; fi
	if [ $2 -eq 2 ]; then 
		count2=`expr $count2 + 1`; newlevel=$count2; 
		second[$count2]=$disksize; fi
	if [ $2 -eq 3 ]; then 
		count3=`expr $count3 + 1`; newlevel=$count3; 
		third[$count3]=$disksize; fi

	from=`expr $1 - 1`
	to=`expr $2 - 1`

	movedisk $from $disksize $level $to $newlevel
	
	if [ $1 -eq 1 ]; then	count1=`expr $count1 - 1`;	fi
	if [ $1 -eq 2 ]; then 	count2=`expr $count2 - 1`;	fi
	if [ $1 -eq 3 ]; then 	count3=`expr $count3 - 1`;	fi

	sleep $S
}

function movetower {
	if [ $1 -gt 0 ];	then
		movetower `expr $1 - 1` $2 $4 $3
		domove $2 $3
		movetower `expr $1 - 1` $4 $3 $2
	fi		
}

clear
tput init
printbars

for i in $(seq 1 $N); do
	printdisk 0 `expr $N - $i + 1` $i
	first[$i]=`expr $N - $i + 1`
	second[$i]=0
	third[$i]=0
done

sleep $S
movetower $N 1 3 2

sleep $S

echo "Finished!"
