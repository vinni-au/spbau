#!/bin/bash

function fib
{
	if [ $1 -eq 0 ]
	then
		return 0
	else
		if [ $1 -eq 1 ]
		then
			return 1
		else
			let "n1 = $n - 1"
			let "n2 = $n - 2"
			
			return l
		fi
	fi
}

echo `fib $1`
