#!/bin/bash

for arg in $*
do
	oldext=`expr match "$arg"  '.*\(.*\..*\)'`
	oldext=${oldext:1}
	newext=`echo "$oldext" | rev`
	echo ${arg/%$oldext/$newext}
done 
