#!/bin/bash

if [ $# -lt 2 ]
then
	echo "usage: dir days"
	exit
fi

ctime=`date +%s`

ls -lR --time-style=+%s $1 | grep ^- | awk '{print $3, $7, $6}' | grep "^`whoami`" | awk -v time="$ctime" '{print $2, $3, (time - $3)/3600/24}' | awk -v days=$2 '{if ($3 > days) print $1}'

