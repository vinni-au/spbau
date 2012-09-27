#!/bin/bash

for arg in $*
do
	if [ $(echo "$arg" | grep -E '(\.tar$)|(\.gz)|(\.bz2)' -) ]
	then
		echo "Showing content of $arg"
		tar -tf $arg
		echo "---------------------------"
	fi

	if [ $(echo "$arg" | grep -E '\.zip' -) ]
	then
		echo "Showing content of $arg"
		unzip -l $arg
		echo "---------------------------"
	fi
done
