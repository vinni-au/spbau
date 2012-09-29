if [ $# -gt 0 ]
then
	str=`echo "$1" | rev`
	if [ "$1" = "$str" ]
	then
		echo "YES"
	else
		echo "NO"
	fi
fi