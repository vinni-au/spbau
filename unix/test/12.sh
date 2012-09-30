#!/bin/bash

if [ $# -ne 3 ]
then
	echo "parameters: day month year"
	exit
fi

ncal $2 $3 | grep -E "\b$1\b" | grep -oE "^.{2}"
