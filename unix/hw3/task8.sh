#!/bin/bash
#Bubble sort

length=0
for arg in $*
do
	array[$length]=$arg
    length=`expr $length + 1`
done


for ((i=0; i < $length; i++))
do
	for ((j=`expr $length - 1`; j > $i; j--))
	do
		if [ ${array[`expr $j - 1`]} -gt ${array[$j]} ]
		then
			t=${array[`expr $j - 1`]}
			array[`expr $j - 1`]=${array[$j]}
			array[$j]=$t
		fi
	done
done

for ((i=0; i < $length; i++))
do
	echo ${array[$i]}
done
