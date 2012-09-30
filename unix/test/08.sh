#!/bin/bash

fib[0]=0
fib[1]=1

for ((i=2; i < 30; i++))
do
	i1=`expr $i - 1`
	i2=`expr $i - 2`
	fib[$i]=`expr ${fib[$i1]} + ${fib[$i2]}`
done

for ((i=0; i < 30; i++))
do
	echo -n "${fib[$i]} ";
done

echo ""
