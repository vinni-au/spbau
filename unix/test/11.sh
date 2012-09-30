#!/bin/bash

if [ $# -eq 1 ]
then
	file="$1"
else 
	exit 0
fi

for mail in `grep -oiE '\b[A-Z0-9._+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b' $file`
do
	count=`echo $mail | grep -o "$mail" $file | wc -l`	
	a[$count]="$mail"
done

for i in ${!a[@]}
do
	str="$str$i ${a[$i]}\n"
done

echo -e $str | tac
