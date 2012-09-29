#!/bin/bash

for arg in `ls`
do
	if [ -f $arg ]
	then
		if [ -h $arg ]
		then
			echo "$arg"
		fi
	fi
done