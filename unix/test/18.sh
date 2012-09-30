#!/bin/bash

for source in `ls -1 *.c`
do
	for file in `grep -E '#include' $source | sed -e '
	s/#include ["<]//
	s/[">]//'`
	do
		if [ ! -e "$file" ]
		then
			echo "ERROR! $file doesn't exist"
			exit -1
		fi
	done
done

echo "All ok"
