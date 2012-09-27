if [ $# -eq 2 ] 
then
	for x in $( seq $2 )
	do
		echo `tr -cd [:alnum:] < /dev/urandom | head -c$1`
	done
else
	echo "Usage: ./task2.sh m n"
fi
