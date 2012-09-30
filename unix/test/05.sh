#!/bin/bash

file="quotes.txt"
if [ $# -eq 1 ]
then
	file="$1"
fi

echo -n "Quote of the day by "
head -1 $file
echo ""
count=`head -2 $file | tail -1`
rand=`expr $RANDOM % $count`
sed "/\* $rand/,/\*/!d" $file | grep -v '^*'
echo ""
