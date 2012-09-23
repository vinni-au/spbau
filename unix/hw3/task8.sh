#!/bin/bash
#Bubble sort

function exch {
	t=$0
	$0=$1
	$1=$t
}

function compexch {
	if [$0 -le $1]
		exch $0 $1
	fi
}


